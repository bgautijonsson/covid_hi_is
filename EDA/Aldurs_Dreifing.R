library(tidyverse)
library(cowplot)
library(scales)
library(lubridate)
library(plotly)
library(cmdstanr)
library(posterior)
library(tidybayes)
library(here)
library(gganimate)
library(data.table)
library(readxl)

theme_set(theme_classic(base_size = 12) + 
              theme(legend.position = "none"))

d <- read_xlsx("Data/aldurshopar.xlsx") %>% 
    janitor::clean_names() %>% 
    filter(smit == "Innanlands") %>% 
    mutate(date = as_date(upphaf_einangrunar)) %>% 
    select(date, age = aldursflokkur_10_80, tegund = smit) %>% 
    count(date, age) %>% 
    complete(age, date, fill = list("n" = 0)) %>% 
    filter(date >= ymd("2020-03-01"))

d %>% 
    mutate(age = ifelse(age == 80, "80+", str_c(age, " - ", age + 9))) %>% 
    ggplot(aes(date, n)) +
    geom_line(dat = d %>% rename(age2 = age), inherit.aes = F,
              aes(x = date, y = n, group = age2), alpha = 0.1) +
    geom_line(aes(col = age, group = age)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = 0)) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    facet_wrap("age") +
    labs(title = "Nýgreind smit eftir aldri",
         x = "Dagsetning",
         y = "Fjöldi") +
    ggsave("smit_aldur.png", device = "png",
           width = 5, height = 5, scale = 2)



d %>% 
    expand_grid(
        tibble(
            wave = c("1", "2", "3"),
            start = ymd(c("2020-03-04", "2020-07-23", "2020-09-11")),
            stop = ymd(c("2020-05-04", "2020-09-10", "2022-01-01"))
        )
    ) %>% 
    filter(date >= start, date <= stop) %>% 
    group_by(age, wave) %>% 
    summarise(n = sum(n)) %>% 
    group_by(wave) %>% 
    mutate(p = n / sum(n)) %>% 
    ggplot(aes(age, p, fill = wave, col = wave, group = wave)) +
    geom_area(alpha = 0.6, position = "identity") +
    scale_y_continuous(labels = label_percent(), limits = c(0, 0.3), breaks = c(0, 0.1, 0.2, 0.3)) +
    scale_x_continuous(labels = function(x) str_c(x, " - ", x + 9),
                       breaks = pretty_breaks(9)) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    coord_cartesian(expand = F) +
    labs(col = "Bylgja", fill = "Bylgja",
         x = "Aldur", y = "Hlutfall smita",
         title = "Aldurdreifing COVID-19 smita",
         subtitle = "Sýnt eftir bylgju") +
    theme(legend.position = c(0.8, 0.8), legend.background = element_rect(colour = "black")) +
    ggsave("aldursdreifing.png", width = 5, height = 0.5 * 5, scale = 2)



age_stats <- tibble(age = unique(d$age)) %>% 
    arrange(age) %>% 
    mutate(hosp = c(0.001, 0.03, 0.012, 0.032, 0.049, 0.102, 0.166, 0.243, 0.273),
           icu = c(0.05, 0.05, 0.05, 0.05, 0.06, 0.12, 0.27, 0.43, 0.709),
           mort = c(0.00002, 0.00006, 0.0003, 0.0008, 0.0015, 0.006, 0.022, 0.051, 0.093))


d %>% 
    filter(date <= Sys.Date() - 10) %>% 
    group_by(age) %>% 
    summarise(n = sum(n)) %>% 
    inner_join(age_stats) %>% 
    mutate(hosp_n = hosp * n,
           icu_n = icu * hosp_n,
           mort_n = mort * n) %>% 
    pivot_longer(c(contains("_n"))) %>% 
    group_by(name) %>% 
    summarise(pred = sum(value))






d %>% 
    filter(date <= Sys.Date() - 10) %>% 
    expand_grid(
        tibble(
            wave = c("1", "2", "3"),
            start = ymd(c("2020-03-04", "2020-07-23", "2020-09-11")),
            stop = ymd(c("2020-05-04", "2020-09-10", "2022-01-01"))
        )
    ) %>% 
    filter(date >= start, date <= stop) %>% 
    group_by(age, wave) %>% 
    summarise(n = sum(n)) %>% 
    inner_join(age_stats) %>% 
    mutate(hosp_n = hosp * n,
           icu_n = icu * hosp_n,
           mort_n = mort * n) %>% 
    pivot_longer(c(contains("_n"))) %>% 
    group_by(wave, name) %>% 
    summarise(value = sum(value))

obs_d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>%
    rename(new_deaths = "Dauðsföll", new_cases = Ny_Smit, n = Smit_Samtals, mort = Dauðsföll_Samtals,
           hosp = "Spitali_Samtals", icu = "Gjorgaesla_Samtals") %>% 
    mutate(date = ymd(Dagsetning),
           country = "Iceland") %>% 
    dplyr::select(date, n, hosp, icu, mort)

d %>% 
    arrange(age, date) %>% 
    complete(age, date, fill = list("n" = 0)) %>% 
    inner_join(age_stats) %>% 
    group_by(age) %>% 
    mutate(hosp = lag(hosp * n, 7, default = 0),
           icu = lag(icu * hosp, 3, default = 0),
           mort = lag(mort * n, 15, default = 0)) %>% 
    pivot_longer(c(-date, -age)) %>% 
    group_by(date, name) %>% 
    summarise(n = sum(value)) %>% 
    group_by(name) %>% 
    mutate(n = cumsum(n)) %>% 
    ggplot(aes(date, n)) +
    geom_line() +
    geom_point(data = obs_d %>% pivot_longer(c(-date), values_to = "n")) +
    scale_y_continuous(breaks = pretty_breaks(8)) +
    facet_wrap("name", scales = "free")


d %>% 
    arrange(age, date) %>% 
    complete(age, date, fill = list("n" = 0)) %>%
    group_by(age) %>% 
    mutate(total = cumsum(n),
           active = total - lag(total, 14, default = 0)) %>% 
    ungroup %>% 
    group_by(date) %>% 
    mutate(p = active / sum(active)) %>% 
    ungroup %>% 
    ggplot(aes(age, p, group = date, fill = date)) +
    geom_area(position = "identity") +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = 0)) +
    scale_x_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0),
                       labels = function(x) ifelse(x == 80, 
                                                   "80+", 
                                                   str_c(x, " - ", x + 9))) +
    scale_fill_distiller(type = "div", palette = "RdBu") +
    transition_time(date) +
    ease_aes() +
    shadow_mark(alpha = 0.05) +
    labs(title = "Aldursdreifing virkra smita (Reiknað með 14 daga lag)",
         subtitle = "Dagsetning: {frame_time}",
         x = "Aldur", y = "Hlutfall")

anim_save("aldursdreifing.gif")


icelandic_dates <- function(x) {
    months <- c("janúar", "febrúar", "mars", "apríl", "maí", "júní", 
                "júlí", "ágúst", "september", "október", "nóvember", "desember")
    
    paste0(months[month(x)])
}

d %>% 
    arrange(age, date) %>% 
    complete(age, date, fill = list("n" = 0)) %>%
    group_by(age) %>% 
    mutate(total = cumsum(n),
           active = total - lag(total, 14, default = 0)) %>% 
    ungroup %>% 
    group_by(date) %>% 
    mutate(p = active / sum(active)) %>% 
    ungroup %>% 
    ggplot(aes(date, p, fill = fct_rev(factor(age)))) +
    geom_area() +
    scale_y_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = 0)) +
    scale_x_date(date_breaks = "month", expand = expansion(add = 0),
                 labels = icelandic_dates) +
    scale_fill_brewer(type = "div", 
                      palette = "RdBu", 
                      guide = guide_legend(keyheight = unit(1.23, "cm")),
                      labels = function(x) ifelse(x == "80", 
                                                  "80+", 
                                                  str_c(as.numeric(as.character(x)), 
                                                        " - ", 
                                                        as.numeric(as.character(x)) + 9))) +
    labs(fill = "Aldur") +
    theme(legend.position = "right",
          axis.title = element_blank()) +
    ggsave("aldursdreifing.png", device = "png",
           width = 5, height = 0.5 * 5, scale = 2)

