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

theme_set(theme_classic(base_size = 12) + 
              theme(legend.position = "none"))

Make_Figures <- function(fit_date = Sys.Date()) {
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>%
        rename(new_deaths = "Dauðsföll", new_cases = Ny_Smit, total_cases = Smit_Samtals, total_deaths = Dauðsföll_Samtals) %>% 
        mutate(date = ymd(Dagsetning),
               country = "Iceland") %>% 
        dplyr::select(country, date, total_cases, new_cases, new_deaths, total_deaths,
                      innanlands = Innanlands_Smit, imported = Innflutt_Smit)
    
    logo <- grid::rasterGrob(png::readPNG("pallas_athena_transparent_grey.png"), interpolate = TRUE)
    
    #### Bylgju línur ####
    plot_dat <- tibble(
        date = seq.Date(from = ymd("2020-02-20"), to = ymd("2020-02-26"), by = "day"),
        new_cases = 0
    ) %>% 
        bind_rows(d %>% select(date, new_cases = innanlands)) %>% 
        mutate(total_cases = cumsum(new_cases),
               rolling_new_cases = data.table::frollmean(new_cases, n = 7, align = "right")) %>% 
        expand_grid(
            tibble(
                wave = c("1", "2"),
                start = ymd(c("2020-03-04", "2020-07-23")),
                stop = ymd(c("2020-05-04", "2022-01-01"))
            )
        ) %>% 
        filter(date >= start, date <= stop) %>% 
        group_by(wave) %>% 
        mutate(total_cases = total_cases - min(total_cases)) %>% 
        mutate(days = row_number() - 1) %>% 
        filter(days < 60) %>% 
        pivot_longer(c(new_cases, total_cases)) %>% 
        mutate(name = fct_recode(name,
                                 "Innlend dagleg smit" = "new_cases",
                                 "Innlend uppsöfnuð smit" = "total_cases")) 
    
    p1 <- plot_dat %>% 
        filter(name == "Innlend dagleg smit") %>% 
        ggplot(aes(days, value, col = wave, group = wave,
                   text = str_c("Bylgja: ", wave, "<br>",
                                "Dagur: ", days, "<br>",
                                "Breyta: ", name, "<br>",
                                "Gildi: ", round(value, 1)))) +
        geom_line(size = 1.1) +
        scale_colour_manual(values = c("#D55E00", "#0072B2")) +
        scale_y_continuous(expand = expansion(mult = 0.01)) +
        scale_x_continuous(breaks = pretty_breaks(5)) +
        labs(colour = "Bylgja",
             x = "Dagar frá upphafi bylgju",
             subtitle = "Innlend dagleg smit") +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              legend.background = element_rect(colour = "black"),
              plot.margin = margin(5, 5, 5, 16))
    
    p2 <- plot_dat %>% 
        filter(name == "Innlend uppsöfnuð smit") %>% 
        ggplot(aes(days, value, col = wave, group = wave,
                   text = str_c("Bylgja: ", wave, "<br>",
                                "Dagur: ", days, "<br>",
                                "Breyta: ", name, "<br>",
                                "Gildi: ", round(value, 1)))) +
        geom_line(data = tibble(days = seq(0, 30, length.out = 100)) %>% 
                      mutate(y = 1.0999 * exp(days * 0.3599)) %>% 
                      filter(y < 2000),
                  aes(x = days, y = y),
                  inherit.aes = F, col = "#e31a1c", size = 1) +
        geom_line(size = 1.1) +
        geom_text(data = tibble(),
                  aes(x = 16, y = 1000, 
                      label = "Óheftur\n veldisvísisvöxtur              "),
                  col = "#e31a1c", inherit.aes = F,
                  size = 4) +
        scale_colour_manual(values = c("#D55E00", "#0072B2")) +
        scale_y_continuous(expand = expansion(mult = 0)) +
        scale_x_continuous(breaks = pretty_breaks(5)) +
        labs(colour = "Bylgja",
             x = "Dagar frá upphafi bylgju",
             title = "Samanburður á bylgjum á Íslandi (Innlend smit)",
             subtitle = "Innlend uppsöfnuð smit") +
        coord_cartesian(ylim = c(0, 1500)) +
        theme(legend.position = c(0.1, 0.8),
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              legend.background = element_rect(colour = "black"),
              plot.margin = margin(5, 5, 5, 4))
    
    plot_grid(p2, p1, ncol = 1) +
        ggsave(here("Results", "Figures", "innlend_smit.png"), width = 5, height = 0.9 * 5, scale = 2)
    
    
    
    #### Bygju súlur ####
    
    plot_dat %>% 
        filter(name == "Innlend uppsöfnuð smit", days <= 30) %>% 
        ggplot(aes(days, value, group = wave, fill = wave)) +
        geom_col(position = "dodge", width = 1, col = "white", alpha = 0.8) +
        scale_fill_manual(values = c("#D55E00", "#0072B2")) +
        scale_y_continuous(expand = expansion(mult = 0)) +
        scale_x_continuous(breaks = pretty_breaks(6), expand = expansion(add = 0.1)) +
        geom_vline(xintercept = as.numeric(ymd("2020-07-31") - ymd("2020-07-23")), lty = 2,
                   col = "#0072B2", size = 1, alpha = 0.8) +
        geom_vline(xintercept = as.numeric(ymd("2020-03-16") - ymd("2020-03-04")), lty = 2,
                   col = "#D55E00", size = 1, alpha = 0.8) +
        geom_vline(xintercept = as.numeric(ymd("2020-03-24") - ymd("2020-03-04")), lty = 2,
                   col = "#D55E00", size = 1, alpha = 0.8) +
        geom_text(data = tibble(), 
                  aes(x = 6.5, y = 600, label = "Samkomur\ntakmarkaðar  \n   við 100 manns         \n      31. júlí"), 
                  colour = "#0072B2",
                  inherit.aes = FALSE, size = 4) +
        geom_text(data = tibble(), 
                  aes(x = 10.5, y = 600, label = "Samkomur\ntakmarkaðar  \n   við 100 manns         \n   16. mars"), 
                  colour = "#D55E00",
                  inherit.aes = FALSE, size = 4) +
        geom_text(data = tibble(), 
                  aes(x = 18.5, y = 600, label = "Samkomur\ntakmarkaðar  \n   við 20 manns       \n   24. mars"), 
                  colour = "#D55E00",
                  inherit.aes = FALSE, size = 4) +
        labs(colour = "Bylgja",
             fill = "Bylgja",
             x = "Dagar frá upphafi bylgju",
             title = "Uppsafnaður fjöldi smita innanlands (COVID-19)") +
        theme(legend.position = c(0.08, 0.85),
              axis.title.y = element_blank(),
              legend.background = element_rect(colour = "black"),
              plot.margin = margin(5, 5, 5, 16)) +
        ggsave(here("Results", "Figures", "samanburdur.png"), device = "png", 
               height = 0.621 * 5, width = 5, scale = 2)
    
    #### Dagleg smit og R_t ####
    
    model_d <- here("Results", "Data", str_c("COVID_Data_", fit_date, ".csv")) %>% read_csv
    
    mod <- here("Results", "Models", "IGLGM", str_c("Model_", fit_date, ".rds")) %>% read_rds
    
    m <- mod$draws() %>% as_draws_df
    
    rm(mod)
    
    par_res <- spread_draws(m, 
                            alpha[wave_id],
                            beta[wave_id], 
                            S[wave_id],
                            nu[wave_id],
                            phi[wave_id]) %>% 
        ungroup %>% 
        group_by(wave_id) %>% 
        mutate(iter = row_number()) %>% 
        ungroup %>% 
        select(-.chain, -.iteration, -.draw)
    
    plot_dat <- model_d %>% filter(location == "Iceland")
    info_dat <- model_d %>% filter(location == "Iceland")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(3) - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    
    obs_results_total <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "total",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    icelandic_dates <- function(x) {
        months <- c("janúar", "febrúar", "mars", "apríl", "maí", "júní", 
                    "júlí", "ágúst", "september", "október", "nóvember", "desember")
        lab <- paste0(mday(x), ". ", months[month(x)])
        
        case_when(lab == "24. mars" ~ "   24. mars",
                  lab == "16. mars" ~ "16. mars   ",
                  TRUE ~ lab)
        
        
        return(lab)
    }
    
    mod_sir <- here("Results", "Models", "RW SIR", str_c("Model_", fit_date, ".rds")) %>% read_rds
    
    m_sir <- mod_sir$draws() %>% as_draws_df
    
    rm(mod_sir)
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>%
        rename(new_deaths = "Dauðsföll", new_cases = Ny_Smit, total_cases = Smit_Samtals, total_deaths = Dauðsföll_Samtals) %>% 
        mutate(date = ymd(Dagsetning),
               country = "Iceland") %>% 
        dplyr::select(country, date, total_cases, new_cases, new_deaths, total_deaths,
                      innanlands = Innanlands_Smit, imported = Innflutt_Smit)
    
    
    p1 <- obs_results_total %>% 
        filter(waves == "total", type == "obs")  %>% 
        group_by(days, date) %>% 
        summarise(lower_50 = quantile(new_cases, .25),
                  upper_50 = quantile(new_cases, .75),
                  lower_60 = quantile(new_cases, .2),
                  upper_60 = quantile(new_cases, .8),
                  lower_70 = quantile(new_cases, .15),
                  upper_70 = quantile(new_cases, .85),
                  lower_80 = quantile(new_cases, .1),
                  upper_80 = quantile(new_cases, .9),
                  lower_90 = quantile(new_cases, .05),
                  upper_90 = quantile(new_cases, .95),
                  lower_95 = quantile(new_cases, .025),
                  upper_95 = quantile(new_cases, .975),
                  .groups = "drop") %>% 
        pivot_longer(c(-days, -date), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob)) %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
        geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
        geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
        geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
        geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
        geom_vline(xintercept = ymd("2020-08-19"), lty = 2) +
        geom_point(data = plot_dat, inherit.aes = F, size = 0.6,
                   aes(date, new_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(breaks = ymd(c("2020-03-01", 
                                    "2020-03-16", "2020-03-24",
                                    "2020-05-04",
                                    "2020-06-15",
                                    "2020-07-31",
                                    "2020-09-01",
                                    "2020-08-16",
                                    "2020-09-16")),
                     labels = icelandic_dates,
                     expand = expansion(add = 0),
                     guide = guide_axis(n.dodge = 1),
                     limits = range(obs_results_total$date) + c(0, 5)) +
        scale_fill_brewer() +
        labs(title = "Dagleg greind smit innanlands") +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-08-15"), xmax = max(obs_results_total$date) + 5,
                          ymin = 90, ymax = 140) +
        theme(axis.title = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.margin = margin(5, 5, 5, 8))
    
    
    p2 <- spread_draws(m_sir, r[day]) %>% 
        ungroup %>% 
        mutate(date = min(d$date) + day + 1) %>%  
        pivot_longer(c(-.iteration, -.chain, -.draw, -day, -date), names_to = "name", values_to = "value") %>%  
        na.omit %>% 
        group_by(date, name) %>% 
        summarise(lower_50 = quantile(value, .25),
                  upper_50 = quantile(value, .75),
                  lower_60 = quantile(value, .2),
                  upper_60 = quantile(value, .8),
                  lower_70 = quantile(value, .15),
                  upper_70 = quantile(value, .85),
                  lower_80 = quantile(value, .1),
                  upper_80 = quantile(value, .9),
                  lower_90 = quantile(value, .05),
                  upper_90 = quantile(value, .95),
                  lower_95 = quantile(value, .025),
                  upper_95 = quantile(value, .975),
                  .groups = "drop") %>% 
        ungroup %>% 
        pivot_longer(c(-date, -name), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob)) %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_hline(aes(yintercept = 1), lty = 1, col = "grey20", alpha = 0.6) +
        geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
        geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
        geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
        geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
        geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
        geom_vline(xintercept = ymd("2020-08-19"), lty = 2) +
        scale_fill_brewer() +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(breaks = ymd(c("2020-03-01", 
                                    "2020-03-16", 
                                    "2020-05-04",
                                    "2020-06-15",
                                    "2020-07-31",
                                    "2020-08-19",
                                    "2020-09-01",
                                    "2020-09-16")),
                     labels = icelandic_dates,
                     expand = expansion(add = 0),
                     limits = range(obs_results_total$date) + c(0, 5)) +
        ggtitle(label = latex2exp::TeX("Þróun smitstuðuls ($R_t$) innanlands")) +
        theme(axis.title = element_blank(), 
              plot.margin = margin(5, 5, 5, 10))
    
    plot_grid(p1, p2, ncol = 1) +
        ggsave(here("Results", "Figures", "spa_og_r.png"), device = "png", 
               height = 0.621 * 5, width = 5, scale = 2.5)
    
    
    #### Spá Ísland ####
    
    plot_dat <- model_d %>% filter(location == "Iceland") %>% filter(wave_id == max(wave_id))
    info_dat <- model_d %>% filter(location == "Iceland") %>% filter(wave_id == max(wave_id))
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(3) - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    set.seed(1)
    obs_results_latest <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "latest",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    pred_dat <- obs_results_latest %>% 
        filter(waves == "latest", type == "obs")  %>% 
        mutate(total_cases = total_cases - min(plot_dat$total_cases),
               total_cases = pmax(total_cases, 0)) %>% 
        pivot_longer(c(new_cases, total_cases)) %>% 
        group_by(days, date, name) %>% 
        summarise(lower_50 = quantile(value, .25),
                  upper_50 = quantile(value, .75),
                  lower_60 = quantile(value, .2),
                  upper_60 = quantile(value, .8),
                  lower_70 = quantile(value, .15),
                  upper_70 = quantile(value, .85),
                  lower_80 = quantile(value, .1),
                  upper_80 = quantile(value, .9),
                  lower_90 = quantile(value, .05),
                  upper_90 = quantile(value, .95),
                  lower_95 = quantile(value, .025),
                  upper_95 = quantile(value, .975),
                  .groups = "drop") %>% 
        pivot_longer(c(-days, -date, -name), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob))
    
    plot_dat <- plot_dat %>% 
        mutate(total_cases = total_cases - min(total_cases) + new_cases[1])
    
    plot_breaks <- ymd(c("2020-07-23", "2020-08-01", "2020-08-16", "2020-09-01", "2020-09-16", "2020-10-01"))
    
    p1 <- pred_dat %>% 
        filter(name == "new_cases") %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_point(data = plot_dat, inherit.aes = F, size = 1,
                   aes(date, new_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(breaks = plot_breaks, labels = icelandic_dates, expand = expansion(add = 0)) +
        scale_fill_brewer() +
        labs(subtitle = "Innlend dagleg smit") +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-09-02"), xmax = fit_date + weeks(3),
                          ymin = 10, ymax = 16) +
        theme(axis.title = element_blank(), 
              plot.margin = margin(5, 5, 5, 16))
    
    
    p2 <- pred_dat %>% 
        filter(name == "total_cases") %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_point(data = plot_dat, inherit.aes = F, size = 1,
                   aes(date, total_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(breaks = plot_breaks, labels = icelandic_dates, expand = expansion(add = 0)) +
        scale_fill_brewer() +
        labs(subtitle = "Innlend uppsöfnuð smit") +
        theme(axis.title = element_blank(), 
              plot.margin = margin(5, 5, 5, 5))
    
    
    
    plot_grid(p1, p2, ncol = 1) +
        ggsave(here("Results", "Figures", "latest_spa.png"), device = "png", 
               height = 0.621 * 5, width = 5, scale = 2)
    
    #### Næstu 3 vikur ####
    
    plot_dat <- model_d %>% filter(location == "Iceland")
    info_dat <- model_d %>% filter(location == "Iceland")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(5) - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    
    
    
    plot_dat <- plot_dat %>% filter(wave_id == max(wave_id))
    info_dat <- info_dat %>% filter(wave_id == max(wave_id))
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(5) - start_date), by = 1)
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    
    
    plot_dat <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        mutate(date = min(plot_dat$date) + days) %>% 
        select(date, iter, new_cases) %>% 
        filter(date > Sys.Date(), date <= Sys.Date() + 21) %>% 
        group_by(iter) %>% 
        summarise(total_cases = sum(new_cases), .groups = "drop") %>% 
        arrange(total_cases) %>% 
        mutate(q = row_number() / n())
    
    quantiles <- plot_dat %>% 
        expand_grid(prob = c(0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975)) %>% 
        group_by(prob) %>% 
        summarise(est = quantile(total_cases, prob)) %>% 
        distinct(prob, est)
    
    
    plot_dat %>% 
        filter(q <= 0.99) %>% 
        ggplot(aes(total_cases, q)) +
        geom_smooth(se = 0, col = "black") +
        geom_segment(data = quantiles, inherit.aes = F,
                     aes(x = est, y = prob, xend = est, yend = 0), lty = 2) +
        geom_segment(data = quantiles, inherit.aes = F,
                     aes(x = est, y = prob, xend = 0, yend = prob), lty = 2) +
        scale_y_continuous(labels = label_percent(accuracy = 0.1), expand = expansion(mult = 0), breaks = unique(quantiles$prob)) +
        scale_x_continuous(expand = expansion(mult = 0), 
                           breaks = unique(quantiles$est),
                           labels = round(unique(quantiles$est))) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(x = "Fjöldi smita næstu þrjár vikur",
             y = "P(X < x)") +
        ggsave(here("Results", "Figures", "smit_naestu_3_vikur.png"), device = "png", 
               height = 0.621 * 5, width = 5, scale = 2.5)
    
    
    
    #### Dreifing GIF ####
    
    plot_dat <- model_d %>% filter(location == "Iceland")
    info_dat <- model_d %>% filter(location == "Iceland")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(Sys.Date() + weeks(8) - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    
    set.seed(1)
    obs_results_total <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "total",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    icelandic_dates <- function(x) {
        months <- c("janúar", "febrúar", "mars", "apríl", "maí", "júní", 
                    "júlí", "ágúst", "september", "október", "nóvember", "desember")
        
        paste0(mday(x), ". ", months[month(x)])
    }
    
    p1 <- obs_results_total %>% 
        filter(date >= Sys.Date(), date <= Sys.Date() + weeks(3)) %>% 
        # count(new_cases, week = floor_date(date, "week", week_start = 1)) %>% 
        mutate(new_cases = pmin(new_cases, 11)) %>% 
        count(new_cases, date) %>% 
        group_by(date) %>% 
        mutate(p = n / sum(n)) %>% 
        mutate(q = cumsum(p)) %>% 
        ggplot(aes(new_cases, p, group = "none")) +
        geom_area(fill = "grey30", col = NA) +
        scale_x_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0),
                           labels = function(x) ifelse(x == 11, ">10", as.character(x))) +
        scale_y_continuous(breaks = seq(0, 1, 0.1), labels = label_percent(accuracy = 1),
                           expand = expansion(add = 0), limits = c(0, 1)) +
        scale_colour_distiller(type = "div") +
        transition_time(date) +
        ease_aes() +
        shadow_mark(alpha = 0.05) +
        annotation_custom(grob = logo, 
                          xmin = 7, xmax = 11,
                          ymin = 0.8, ymax = 1) +
        labs(title = "Dreifing daglegra smita næstu þrjár vikurnar",
             subtitle = "Dagsetning: {icelandic_dates(frame_time)}",
             x = "Fjöldi greindra smita",
             y = "Líkur") +
        theme(plot.margin = margin(5, 15, 5, 5))
    
    animate(p1, nframes = 40, width = 600, height = 600)
    
    anim_save(here("Results", "Figures", "dreifing.gif"))
    
    p1 <- obs_results_total %>% 
        filter(date >= Sys.Date(), date <= Sys.Date() + weeks(3)) %>% 
        # count(new_cases, week = floor_date(date, "week", week_start = 1)) %>% 
        count(new_cases, date) %>% 
        group_by(date) %>% 
        mutate(p = n / sum(n)) %>% 
        mutate(q = cumsum(p)) %>% 
        filter(new_cases <= 10) %>% 
        ggplot(aes(new_cases, 1 - q, group = "none")) +
        geom_area(fill = "grey30", col = NA) +
        scale_x_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0),
                           labels = function(x) ifelse(x == 11, ">10", as.character(x))) +
        scale_y_continuous(breaks = seq(0, 1, 0.1), labels = label_percent(accuracy = 1),
                           expand = expansion(add = 0), limits = c(0, 1)) +
        scale_colour_distiller(type = "div") +
        transition_time(date) +
        ease_aes() +
        shadow_mark(alpha = 0.05) +
        annotation_custom(grob = logo, 
                          xmin = 7, xmax = 11,
                          ymin = 0.8, ymax = 1) +
        labs(title = "Dreifing lágmarksfjölda daglegra smita næstu þrjár vikurnar",
             subtitle = "Dagsetning: {icelandic_dates(frame_time)}",
             x = "Fjöldi greindra smita",
             y = "Líkur á að sjá fleiri en X smit") +
        theme(plot.margin = margin(5, 15, 5, 5))
    
    animate(p1, nframes = 40, width = 600, height = 600)
    
    anim_save(here("Results", "Figures", "survival.gif"))
    
    #### Aus og Isr ####
    
    
    plot_dat <- model_d %>% filter(location == "Australia")
    info_dat <- model_d %>% filter(location == "Australia")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(3) + 2 - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    
    obs_results_total <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "total",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    
    p1 <- obs_results_total %>% 
        filter(waves == "total", type == "obs")  %>% 
        group_by(days, date) %>% 
        summarise(lower_50 = quantile(new_cases, .25),
                  upper_50 = quantile(new_cases, .75),
                  lower_60 = quantile(new_cases, .2),
                  upper_60 = quantile(new_cases, .8),
                  lower_70 = quantile(new_cases, .15),
                  upper_70 = quantile(new_cases, .85),
                  lower_80 = quantile(new_cases, .1),
                  upper_80 = quantile(new_cases, .9),
                  lower_90 = quantile(new_cases, .05),
                  upper_90 = quantile(new_cases, .95),
                  lower_95 = quantile(new_cases, .025),
                  upper_95 = quantile(new_cases, .975),
                  .groups = "drop") %>% 
        pivot_longer(c(-days, -date), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob)) %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_point(data = plot_dat, inherit.aes = F, size = 0.6,
                   aes(date, new_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(date_breaks = "month",
                     labels = icelandic_dates,
                     expand = expansion(add = 0),
                     guide = guide_axis(n.dodge = 1),
                     limits = c(ymd("2020-03-17"), fit_date + weeks(3) + 4)) +
        scale_fill_brewer() +
        labs(title = "Fjöldi daglegra smita í öðrum löndum",
             subtitle = "Ástralia") +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-08-20"), xmax = fit_date + weeks(3) + 3,
                          ymin = 650, ymax = 850) +
        coord_cartesian(clip = "off") +
        theme(axis.title = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.margin = margin(5, 5, 5, 8))
    
    plot_dat <- model_d %>% filter(location == "Israel")
    info_dat <- model_d %>% filter(location == "Israel")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(3) + 2 - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    
    obs_results_total <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "total",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    p2 <- obs_results_total %>% 
        filter(waves == "total", type == "obs")  %>% 
        group_by(days, date) %>% 
        summarise(lower_50 = quantile(new_cases, .25),
                  upper_50 = quantile(new_cases, .75),
                  lower_60 = quantile(new_cases, .2),
                  upper_60 = quantile(new_cases, .8),
                  lower_70 = quantile(new_cases, .15),
                  upper_70 = quantile(new_cases, .85),
                  lower_80 = quantile(new_cases, .1),
                  upper_80 = quantile(new_cases, .9),
                  lower_90 = quantile(new_cases, .05),
                  upper_90 = quantile(new_cases, .95),
                  lower_95 = quantile(new_cases, .025),
                  upper_95 = quantile(new_cases, .975),
                  .groups = "drop") %>% 
        pivot_longer(c(-days, -date), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob)) %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_point(data = plot_dat, inherit.aes = F, size = 0.6,
                   aes(date, new_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(date_breaks = "month",
                     labels = icelandic_dates,
                     expand = expansion(add = 0),
                     guide = guide_axis(n.dodge = 1),
                     limits = c(ymd("2020-03-17"), fit_date + weeks(3) + 4)) +
        scale_fill_brewer() +
        labs(subtitle = "Ísrael") +
        theme(axis.title = element_blank(),
              plot.margin = margin(5, 5, 5, 8))
    
    
    plot_grid(p1, p2, ncol = 1) +
        ggsave(here("Results", "Figures", "aus_isr.png"), device = "png", 
               height = 0.621 * 5, width = 5, scale = 2)
    
    
    #### Den og Ger ####
    
    
    plot_dat <- model_d %>% filter(location == "Denmark")
    info_dat <- model_d %>% filter(location == "Denmark")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(3) + 2 - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    set.seed(1)
    obs_results_total <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "total",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    
    p1 <- obs_results_total %>% 
        filter(waves == "total", type == "obs")  %>% 
        group_by(days, date) %>% 
        summarise(lower_50 = quantile(new_cases, .25),
                  upper_50 = quantile(new_cases, .75),
                  lower_60 = quantile(new_cases, .2),
                  upper_60 = quantile(new_cases, .8),
                  lower_70 = quantile(new_cases, .15),
                  upper_70 = quantile(new_cases, .85),
                  lower_80 = quantile(new_cases, .1),
                  upper_80 = quantile(new_cases, .9),
                  lower_90 = quantile(new_cases, .05),
                  upper_90 = quantile(new_cases, .95),
                  lower_95 = quantile(new_cases, .025),
                  upper_95 = quantile(new_cases, .975),
                  .groups = "drop") %>% 
        pivot_longer(c(-days, -date), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob)) %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_point(data = plot_dat, inherit.aes = F, size = 0.6,
                   aes(date, new_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(date_breaks = "month",
                     labels = icelandic_dates,
                     expand = expansion(add = 0),
                     guide = guide_axis(n.dodge = 1),
                     limits = c(ymd("2020-03-17"), fit_date + weeks(3) + 4)) +
        scale_fill_brewer() +
        labs(title = "Fjöldi daglegra smita í öðrum löndum",
             subtitle = "Danmörk") +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-08-17"), xmax = fit_date + weeks(3) + 3,
                          ymin = 850, ymax = 1200) +
        coord_cartesian(clip = "off") +
        theme(axis.title = element_blank(), 
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.margin = margin(5, 5, 5, 8))
    
    plot_dat <- model_d %>% filter(location == "Germany")
    info_dat <- model_d %>% filter(location == "Germany")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(3) + 2 - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    set.seed(1)
    obs_results_total <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "total",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    p2 <- obs_results_total %>% 
        filter(waves == "total", type == "obs")  %>% 
        group_by(days, date) %>% 
        summarise(lower_50 = quantile(new_cases, .25),
                  upper_50 = quantile(new_cases, .75),
                  lower_60 = quantile(new_cases, .2),
                  upper_60 = quantile(new_cases, .8),
                  lower_70 = quantile(new_cases, .15),
                  upper_70 = quantile(new_cases, .85),
                  lower_80 = quantile(new_cases, .1),
                  upper_80 = quantile(new_cases, .9),
                  lower_90 = quantile(new_cases, .05),
                  upper_90 = quantile(new_cases, .95),
                  lower_95 = quantile(new_cases, .025),
                  upper_95 = quantile(new_cases, .975),
                  .groups = "drop") %>% 
        pivot_longer(c(-days, -date), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob)) %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_point(data = plot_dat, inherit.aes = F, size = 0.6,
                   aes(date, new_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(date_breaks = "month",
                     labels = icelandic_dates,
                     expand = expansion(add = 0),
                     guide = guide_axis(n.dodge = 1),
                     limits = c(ymd("2020-03-11"), fit_date + weeks(3) + 4)) +
        scale_fill_brewer() +
        labs(subtitle = "Þýskaland") +
        theme(axis.title = element_blank(),
              plot.margin = margin(5, 5, 5, 8))
    
    
    plot_grid(p1, p2, ncol = 1) +
        ggsave(here("Results", "Figures", "den_ger.png"), device = "png", 
               height = 0.621 * 5, width = 5, scale = 2)
    
    #### UK ####
    
    
    plot_dat <- model_d %>% filter(location == "United Kingdom")
    info_dat <- model_d %>% filter(location == "United Kingdom")
    
    location_id <- unique(info_dat$location_id)
    wave_id <- unique(info_dat$wave_id)
    pop <- unique(info_dat$population)
    start_cases <- min(info_dat$total_cases)
    
    start_date <- min(info_dat$date)
    
    end_date <- max(info_dat$date)
    
    
    days_in_data <- max(info_dat$days) + 1
    
    
    day_seq <- seq(0, as.numeric(fit_date + weeks(3) + 2 - start_date), by = 1)
    
    
    join_dat <- info_dat %>% 
        group_by(location_id, wave_id) %>% 
        summarise(start_day = min(days),
                  end_day = max(days), .groups = "drop") %>% 
        ungroup %>% 
        mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
               end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
    
    obs_results_total <- par_res %>% 
        inner_join(join_dat, by = "wave_id") %>% 
        expand_grid(days = day_seq) %>% 
        filter(days >= start_day, days < end_day) %>% 
        mutate(z = beta * (days - alpha - start_day),
               f = S - S / (1 + nu * exp(z))^(1/nu),
               dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
               new_cases = rnbinom(n(), mu = dfdt * pop, size = phi)) %>% 
        group_by(iter) %>% 
        mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
        ungroup %>% 
        mutate(type = "obs", waves = "total",
               date = min(plot_dat$date) + days) %>% 
        select(iter, days, date, new_cases, total_cases, type, waves)
    
    
    p1 <- obs_results_total %>% 
        filter(waves == "total", type == "obs")  %>% 
        group_by(days, date) %>% 
        summarise(lower_50 = quantile(new_cases, .25),
                  upper_50 = quantile(new_cases, .75),
                  lower_60 = quantile(new_cases, .2),
                  upper_60 = quantile(new_cases, .8),
                  lower_70 = quantile(new_cases, .15),
                  upper_70 = quantile(new_cases, .85),
                  lower_80 = quantile(new_cases, .1),
                  upper_80 = quantile(new_cases, .9),
                  lower_90 = quantile(new_cases, .05),
                  upper_90 = quantile(new_cases, .95),
                  lower_95 = quantile(new_cases, .025),
                  upper_95 = quantile(new_cases, .975),
                  .groups = "drop") %>% 
        pivot_longer(c(-days, -date), names_to = c("which", "prob"), names_sep = "_") %>% 
        pivot_wider(names_from = which, values_from = value) %>% 
        mutate(prob = parse_number(prob)) %>% 
        ggplot(aes(date, ymin = lower, ymax = upper)) +
        geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
        geom_point(data = plot_dat, inherit.aes = F, size = 0.6,
                   aes(date, new_cases)) +
        scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
        scale_x_date(date_breaks = "month",
                     labels = icelandic_dates,
                     expand = expansion(add = 0),
                     guide = guide_axis(n.dodge = 1),
                     limits = c(ymd("2020-03-17"), fit_date + weeks(3) + 4)) +
        scale_fill_brewer() +
        labs(title = "Fjöldi daglegra smita í öðrum löndum",
             subtitle = "Bretland") +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-08-20"), xmax = fit_date + weeks(3) + 3,
                          ymin = 5000, ymax = 7000) +
        coord_cartesian(clip = "off") +
        theme(axis.title = element_blank(), 
              plot.margin = margin(5, 5, 5, 8))
    
    
    p1 +
        ggsave(here("Results", "Figures", "uk.png"), device = "png", 
               height = 0.4 * 5, width = 5, scale = 2)
    
    
    #### Nýgengi ####
    
    source("Make_Stan_Data.R")
    
    model_d <- Make_Stan_Data(min_case_rate = 0, return_list = F, save_df = F)
    
    model_d %>% 
        select(location, new_cases, date, population) %>% 
        group_by(location) %>% 
        mutate(biweekly_cases = frollsum(new_cases, n = 14, align = "right", fill = 0),
               biweekly_incidence = biweekly_cases / population * 100000) %>% 
        group_by(location) %>% 
        mutate(y = ifelse(row_number() == n(), biweekly_incidence[n()], NA)) %>% 
        ungroup %>% 
        filter(location %in% c("France", "Germany", "Iceland")) %>%  
        mutate(location = fct_recode(location,
                                     "Frakkland" = "France",
                                     "Þýskaland" = "Germany",
                                     "Ísland" = "Iceland") %>% 
                   fct_relevel("Frakkland", "Ísland")) %>% 
        ggplot(aes(date, biweekly_incidence, group = location, colour = location)) +
        geom_line(size = 0.9) +
        geom_text(aes(x = date + 15, y = y, label = location), size = 5) +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-08-25"), ymd("2020-09-25"),
                          ymin = 180, ymax = 230) +
        scale_colour_brewer(type = "qual", palette = "Set1") +
        scale_x_date(limits = c(ymd("2020-02-25"), Sys.Date() + 28), 
                     labels = icelandic_dates,
                     date_breaks = "month") +
        scale_y_continuous(limits = c(0, 250), expand = expansion(mult = 0)) +
        theme(axis.title = element_blank()) +
        ggsave(here("Results", "Figures", "incidence_fra_ger.png"), device = "png",
               width = 5, height = 0.621 * 5, scale = 2)
    
    
    
    model_d %>% 
        select(location, new_cases, date, population) %>% 
        group_by(location) %>% 
        mutate(biweekly_cases = frollsum(new_cases, n = 14, align = "right"),
               biweekly_incidence = biweekly_cases / population * 100000) %>% 
        group_by(location) %>% 
        mutate(y = ifelse(row_number() == n(), biweekly_incidence[n()], NA),
               y = case_when(location == "Sweden" ~ y + 10,
                             location == "Denmark" ~ y + 8,
                             location == "Iceland" ~ y + 3,
                             location == "Norway" ~ y + 3,
                             location == "Finland" ~ y + 1,
                             TRUE ~ y)) %>% 
        ungroup %>% 
        filter(location %in% c("Norway", "Denmark", "Finland", "Sweden", "Iceland")) %>%  
        mutate(location = fct_recode(location,
                                     "Svíþjóð" = "Sweden",
                                     "Danmörk" = "Denmark",
                                     "Ísland" = "Iceland",
                                     "Noregur" = "Norway",
                                     "Finnland" = "Finland"),
               location = fct_relevel(location, "Danmörk", "Finnland", "Ísland")) %>% 
    ggplot(aes(date, biweekly_incidence, group = location, colour = location)) +
        geom_line(size = 0.9) +
        geom_text(aes(x = date + 15, y = y, label = location), size = 5) +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-08-25"), ymd("2020-09-25"),
                          ymin = 180, ymax = 230) +
        scale_colour_brewer(type = "qual", palette = "Set1") +
        scale_x_date(limits = c(ymd("2020-02-25"), Sys.Date() + 25), 
                     labels = icelandic_dates,
                     date_breaks = "month") +
        scale_y_continuous(limits = c(0, 250), expand = expansion(mult = 0)) +
        theme(axis.title = element_blank()) +
        ggsave(here("Results", "Figures", "incidence_nordic.png"), device = "png",
               width = 5, height = 0.621 * 5, scale = 2)
    
    
    model_d %>% 
        select(location, new_cases, date, population) %>% 
        group_by(location) %>% 
        mutate(biweekly_cases = frollsum(new_cases, n = 14, align = "right"),
               biweekly_incidence = biweekly_cases / population * 100000) %>% 
        group_by(location) %>% 
        mutate(y = ifelse(row_number() == n(), biweekly_incidence[n()], NA),
               y = case_when(location == "Spain" ~ y,
                             location == "United Kingdom" ~ y + 5,
                             location == "Italy" ~ y - 4,
                             location == "Iceland" ~ y + 1,
                             TRUE ~ y),
               x = case_when(row_number() != n() ~ NA_Date_,
                             TRUE ~ date),
               x = case_when(location == "Spain" ~ x + 13,
                             location == "United Kingdom" ~ x + 10,
                             location == "Italy" ~ x + 9,
                             location == "Iceland" ~ x + 9,
                             TRUE ~ x)) %>% 
        ungroup %>% 
        filter(location %in% c("Italy", "Spain", "United Kingdom", "Iceland")) %>%  
        mutate(location = fct_recode(location,
                                     "Spánn" = "Spain",
                                     "Bretland" = "United Kingdom",
                                     "Ítalía" = "Italy",
                                     "Ísland" = "Iceland"),
               location = fct_relevel(location, "Spánn", "Ísland")) %>% 
        ggplot(aes(date, biweekly_incidence, group = location, colour = location)) +
        geom_line(size = 0.9) +
        geom_text(aes(x = x, y = y, label = location), size = 5) +
        annotation_custom(grob = logo, 
                          xmin = ymd("2020-08-25"), ymd("2020-09-25"),
                          ymin = 290, ymax = 340) +
        scale_colour_brewer(type = "qual", palette = "Set1") +
        scale_x_date(limits = c(ymd("2020-02-25"), Sys.Date() + 20), 
                     labels = icelandic_dates,
                     date_breaks = "month") +
        scale_y_continuous(limits = c(0, 350), expand = expansion(mult = 0)) +
        theme(axis.title = element_blank()) +
        ggsave(here("Results", "Figures", "incidence_spain_uk_italy.png"), device = "png",
               width = 5, height = 0.621 * 5, scale = 2)
    
}