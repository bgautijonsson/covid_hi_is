library(cmdstanr)
library(rstan)
library(posterior)
library(broom.mixed)
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




mod <- read_rds(here("Results", "Models", "EpiEstim", str_c("Model_", Sys.Date(), ".rds")))
m <- mod$draws() %>% as_draws_df
rm(mod)

d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>%
    select(date = Dagsetning, local = Innanlands_Smit, imported = Innflutt_Smit,
           rop_quarantine=Hlutf_Sottkvi,num_quarantine=Fjoldi_Sottkvi) %>% 
    mutate(date = ymd(date),
           total = if_else(date >= ymd("2020-07-23"),local,local + imported),
           prop_quarantine=if_else(total!=0,num_quarantine/total,0)) %>% 
    filter(date >= ymd("2020-02-28"))


icelandic_dates <- function(x) {
    months <- c("janúar", "febrúar", "mars", "apríl", "maí", "júní", 
                "júlí", "ágúst", "september", "október", "nóvember", "desember")
    
    paste0(mday(x), ". ", months[month(x)])
}

p1 <- d %>% 
    rename(Local = local, Imported = imported) %>% 
    pivot_longer(c(Local, Imported)) %>% 
    ggplot(aes(date, value, fill = name)) +
    geom_col(width = 1, alpha = 0.8) +
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
    geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
    geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
    geom_vline(xintercept = ymd("2020-08-19"), lty = 2) +
    geom_vline(xintercept = ymd("2020-09-07"), lty = 2) +
    scale_fill_brewer(type = "qual", palette = "Set1") +
    scale_x_date(breaks = c(ymd(c("2020-03-01", 
                                  "2020-03-16", "2020-03-24",
                                  "2020-05-04",
                                  "2020-06-15",
                                  "2020-07-01",
                                  "2020-07-31",
                                  "2020-08-19",
                                  "2020-09-07"))),
                 date_labels = "%B %d",
                 expand = expansion(add = 0),
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1)) +
    scale_y_continuous(breaks = pretty_breaks(5), expand = expansion(mult = 0), limits = c(0, 110)) +
    labs(subtitle = "Incidence") +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(5, 5, 5, 5),
          legend.position = c(0.28, 0.8), 
          legend.title = element_blank())

p2 <- spread_draws(m, R[day]) %>% 
    group_by(day) %>% 
    summarise(lower_50 = quantile(R, 0.25),
              upper_50 = quantile(R, 0.75),
              lower_60 = quantile(R, 0.2),
              upper_60 = quantile(R, 0.8),
              lower_70 = quantile(R, 0.15),
              upper_70 = quantile(R, 0.85),
              lower_80 = quantile(R, 0.1),
              upper_80 = quantile(R, 0.9),
              lower_90 = quantile(R, 0.05),
              upper_90 = quantile(R, 0.95),
              lower_95 = quantile(R, 0.025),
              upper_95 = quantile(R, 0.975)) %>% 
    pivot_longer(c(-day), names_to = c("which", "prob"), names_sep = "_") %>% 
    pivot_wider(names_from = which, values_from = value) %>% 
    mutate(prob = parse_number(prob),
           date = ymd("2020-02-28") + day) %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
    geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
    geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
    geom_vline(xintercept = ymd("2020-08-19"), lty = 2) +
    geom_vline(xintercept = ymd("2020-09-07"), lty = 2) +
    geom_hline(yintercept = 1, lty = 2) +
    scale_fill_brewer() +
    scale_x_date(breaks = c(ymd(c("2020-03-01", 
                                  "2020-03-16", "2020-03-24",
                                  "2020-05-04",
                                  "2020-06-15",
                                  "2020-07-01",
                                  "2020-07-31",
                                  "2020-08-19",
                                  "2020-09-07"))),
                 date_labels = "%B %d",
                 expand = expansion(add = 0),
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1)) +
    scale_y_continuous(breaks = pretty_breaks(8)) +
    ggtitle(label = waiver(),
            subtitle = latex2exp::TeX("$R_t$")) +
    theme(axis.title = element_blank(),
          plot.margin = margin(5, 5, 5, 8))

p2b <- spread_draws(m, R[day]) %>% 
    arrange(day) %>% 
    group_by(.chain, .iteration, .draw) %>% 
    mutate(change = exp(diff(c(NA, log(R)))) - 1) %>% 
    drop_na(change) %>% 
    group_by(day) %>% 
    summarise(lower_50 = quantile(change, 0.25),
              upper_50 = quantile(change, 0.75),
              lower_60 = quantile(change, 0.2),
              upper_60 = quantile(change, 0.8),
              lower_70 = quantile(change, 0.15),
              upper_70 = quantile(change, 0.85),
              lower_80 = quantile(change, 0.1),
              upper_80 = quantile(change, 0.9),
              lower_90 = quantile(change, 0.05),
              upper_90 = quantile(change, 0.95),
              lower_95 = quantile(change, 0.025),
              upper_95 = quantile(change, 0.975)) %>% 
    pivot_longer(c(-day), names_to = c("which", "prob"), names_sep = "_") %>% 
    pivot_wider(names_from = which, values_from = value) %>% 
    mutate(prob = parse_number(prob),
           date = ymd("2020-02-28") + day) %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
    geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
    geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
    geom_vline(xintercept = ymd("2020-08-19"), lty = 2) +
    geom_vline(xintercept = ymd("2020-09-07"), lty = 2) +
    geom_hline(yintercept = 0, lty = 2) +
    scale_fill_brewer() +
    scale_x_date(breaks = c(ymd(c("2020-03-01", 
                                  "2020-03-16", "2020-03-24",
                                  "2020-05-04",
                                  "2020-06-15",
                                  "2020-07-01",
                                  "2020-07-31",
                                  "2020-08-19",
                                  "2020-09-07"))),
                 date_labels = "%B %d",
                 expand = expansion(add = 0),
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1)) +
    scale_y_continuous(breaks = pretty_breaks(8), labels = label_percent()) +
    ggtitle(label = waiver(),
            subtitle = latex2exp::TeX("Between-day % change in $R_t$")) +
    theme(axis.title = element_blank(),
          plot.margin = margin(5, 5, 5, 8))



plot_grid(p1, p2, ncol = 1)

plot_grid(p1, p2, p2b, ncol = 1)


p3 <- spread_draws(m, y_hat[day]) %>% 
    group_by(day) %>% 
    summarise(lower_50 = quantile(y_hat, 0.25),
              upper_50 = quantile(y_hat, 0.75),
              lower_60 = quantile(y_hat, 0.2),
              upper_60 = quantile(y_hat, 0.8),
              lower_70 = quantile(y_hat, 0.15),
              upper_70 = quantile(y_hat, 0.85),
              lower_80 = quantile(y_hat, 0.1),
              upper_80 = quantile(y_hat, 0.9),
              lower_90 = quantile(y_hat, 0.05),
              upper_90 = quantile(y_hat, 0.95),
              lower_95 = quantile(y_hat, 0.025),
              upper_95 = quantile(y_hat, 0.975)) %>% 
    pivot_longer(c(-day), names_to = c("which", "prob"), names_sep = "_") %>% 
    pivot_wider(names_from = which, values_from = value) %>% 
    mutate(prob = parse_number(prob),
           date = ymd("2020-02-28") + day) %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_point(
        data = d, aes(x = date, y = local), inherit.aes = F
    ) +
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
    geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
    geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
    geom_vline(xintercept = ymd("2020-08-19"), lty = 2) +
    geom_vline(xintercept = ymd("2020-09-07"), lty = 2) +
    scale_fill_brewer() +
    scale_x_date(breaks = c(ymd(c("2020-03-01", 
                                  "2020-03-16", "2020-03-24",
                                  "2020-05-04",
                                  "2020-06-15",
                                  "2020-07-01",
                                  "2020-07-31",
                                  "2020-08-19",
                                  "2020-09-07"))),
                 date_labels = "%B %d",
                 expand = expansion(add = 0),
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1)) +
    scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0), limits = c(0, 145)) +
    labs(subtitle = "Retrofitted local cases") +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(5, 5, 5, 22),
          legend.title = element_blank()) 

p4 <- spread_draws(m, R[day]) %>% 
    group_by(day) %>% 
    summarise(lower_50 = quantile(R, 0.25),
              upper_50 = quantile(R, 0.75),
              lower_60 = quantile(R, 0.2),
              upper_60 = quantile(R, 0.8),
              lower_70 = quantile(R, 0.15),
              upper_70 = quantile(R, 0.85),
              lower_80 = quantile(R, 0.1),
              upper_80 = quantile(R, 0.9),
              lower_90 = quantile(R, 0.05),
              upper_90 = quantile(R, 0.95),
              lower_95 = quantile(R, 0.025),
              upper_95 = quantile(R, 0.975)) %>% 
    pivot_longer(c(-day), names_to = c("which", "prob"), names_sep = "_") %>% 
    pivot_wider(names_from = which, values_from = value) %>% 
    mutate(prob = parse_number(prob),
           date = ymd("2020-02-28") + day) %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
    geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
    geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
    geom_vline(xintercept = ymd("2020-08-19"), lty = 2) +
    geom_vline(xintercept = ymd("2020-09-07"), lty = 2) +
    scale_fill_brewer() +
    scale_x_date(breaks = c(ymd(c("2020-03-01", 
                                  "2020-03-16", "2020-03-24",
                                  "2020-05-04",
                                  "2020-06-15",
                                  "2020-07-01",
                                  "2020-07-31",
                                  "2020-08-19",
                                  "2020-09-07"))),
                 date_labels = "%B %d",
                 expand = expansion(add = 0),
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1)) +
    scale_y_continuous(breaks = pretty_breaks(8), limits = c(0, NA), expand = expansion(mult = 0)) +
    ggtitle(label = waiver(),
            subtitle = latex2exp::TeX("$R_t$")) +
    theme(axis.title = element_blank(),
          plot.margin = margin(5, 5, 5, 25))

plot_grid(p3, p4, p2b, ncol = 1) +
    ggsave("Rt_plot.png", device = "png",
           width = 5, height =  5, scale = 2)


plot_dat <- spread_draws(m, R[day]) %>% 
    group_by(day) %>% 
    summarise(under_one = mean(R < 1)) %>% 
    mutate(date = ymd("2020-02-28") + day)

p4b <- plot_dat %>% 
    ggplot(aes(date, under_one)) +
    geom_area(fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 15/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 13/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 11/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 9/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 7/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 5/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 3/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 14/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 12/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 10/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 8/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 6/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 4/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 2/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    geom_area(data = plot_dat %>% mutate(under_one = ifelse(under_one < 1/16, 0, under_one)),
              fill = "#08519c", alpha = 1/8) +
    # geom_col(aes(fill = -under_one), width = 1) +
    geom_vline(xintercept = ymd("2020-03-16"), lty = 2) +
    geom_vline(xintercept = ymd("2020-03-24"), lty = 2) +
    geom_vline(xintercept = ymd("2020-05-04"), lty = 2) +
    geom_vline(xintercept = ymd("2020-06-15"), lty = 2) +
    geom_vline(xintercept = ymd("2020-07-31"), lty = 2) +
    scale_fill_distiller() +
    scale_x_date(date_breaks = "month",
                 date_labels = "%B %d",
                 expand = expansion(add = 0),
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1)) +
    scale_y_continuous(breaks = pretty_breaks(5), labels = label_percent(), 
                       limits = c(0, 1.01), expand = expansion(mult = 0)) +
    ggtitle(label = waiver(),
            subtitle = latex2exp::TeX("$P(R_t < 1)$")) +
    theme(axis.title = element_blank(),
          plot.margin = margin(5, 5, 5, 3))


plot_grid(p3, p4, p4b, ncol = 1)

spread_draws(m, R[day], phi) %>% 
    ungroup %>% 
    mutate(infected_by_one = rnbinom(n(), mu = 1 * R, size = phi)) %>% 
    group_by(day) %>% 
    summarise(lower_50 = quantile(infected_by_one, 0.25),
              upper_50 = quantile(infected_by_one, 0.75),
              lower_60 = quantile(infected_by_one, 0.2),
              upper_60 = quantile(infected_by_one, 0.8),
              lower_70 = quantile(infected_by_one, 0.15),
              upper_70 = quantile(infected_by_one, 0.85),
              lower_80 = quantile(infected_by_one, 0.1),
              upper_80 = quantile(infected_by_one, 0.9),
              lower_90 = quantile(infected_by_one, 0.05),
              upper_90 = quantile(infected_by_one, 0.95),
              lower_95 = quantile(infected_by_one, 0.025),
              upper_95 = quantile(infected_by_one, 0.975)) %>% 
    pivot_longer(c(-day), names_to = c("which", "prob"), names_sep = "_") %>% 
    pivot_wider(names_from = which, values_from = value) %>% 
    mutate(prob = parse_number(prob),
           date = ymd("2020-02-28") + day) %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_hline(yintercept = 1, lty = 2) +
    scale_fill_brewer() +
    scale_x_date(date_breaks = "month",
                 date_labels = "%B %d",
                 expand = expansion(add = 0),
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1)) +
    scale_y_continuous(breaks = pretty_breaks(8), limits = c(0, NA), expand = expansion(mult = 0)) +
    labs(title = "Líkindadreifing yfir fjölda afleiddra smita frá hverju smiti",
         subtitle = "Fendið með úrtökum úr viðeigandi neikvæðri tvíkostadreifingu") +
    theme(axis.title = element_blank(),
          plot.margin = margin(5, 5, 5, 14)) +
    ggsave("afleidd_smit.png", device = "png",
           width = 5, height = 0.5 * 5, scale = 2)


R_draws <- spread_draws(m, R[day]) %>% 
    group_by(day) %>% 
    mutate(iter = row_number()) %>%
    ungroup %>% 
    select(iter, day, R)

last_R <- R_draws %>% 
    filter(day == max(day)) %>% 
    .$R

pred_days <- 56
R_days <- 14
N_iter <- 4000
final_R <- 0.4

future_R <- crossing(day = seq_len(pred_days),
                     iter = seq_len(N_iter)) %>% 
    group_by(iter) %>% 
    mutate(R = last_R[iter],
           log_R = log(R),
           log_R = log_R + 0.1 * day,
           perc = pmin((day - 1) / rnorm(1, mean = R_days, sd = 2), rnorm(1, mean = 1, sd = 0.02)),
           log_R = log_R - perc^1.5 * (log_R - log(final_R)),
           R = exp(log_R),
           day = max(R_draws$day) + day) %>%
    ungroup %>% 
    select(-log_R, -perc)

future_R %>% 
    ggplot(aes(day - min(day), R, group = iter)) +
    geom_line(alpha = 0.1) +
    geom_hline(yintercept = 1, lty = 2)



R_draws %>% 
    bind_rows(future_R) %>% 
    ggplot(aes(day, R, group = iter)) +
    geom_line(alpha = 0.01) +
    geom_hline(yintercept = 1, lty = 2)

shape <- 1.54
rate <- 0.28
N_days <- nrow(d)
SI_dat <- tibble(t = seq(1, N_days)) %>% 
    mutate(
        p = case_when(
            TRUE ~ pgamma(t + 0.5, shape = shape, rate = rate) - pgamma(t - 0.5, shape = shape, rate = rate)
        ),
        p = p / sum(p)
    )

SI <- SI_dat$p


lambda1 <- numeric(N_days)
lambda2 <- numeric(N_days)

for (t in 2:N_days) {
    lambda1[t] <- t(head((1 - d$prop_quarantine) * d$total, t - 1)) %*% tail(rev(SI), t - 1) / sum(tail(rev(SI), t - 1))
    lambda2[t] <- t(head(d$prop_quarantine * d$total, t - 1)) %*% tail(rev(SI), t - 1) / sum(tail(rev(SI), t - 1))
}

lambda1 <- c(lambda1, rep(0, pred_days))
lambda2 <- c(lambda2, rep(0, pred_days))



make_preds <- function(d, ...) {
    perc_q <- unique(d$perc_q2)
    for (t in seq(N_days, nrow(d))) {
        d$lambda1[t] <- t((1 - perc_q) * d$mu_hat[(t - 1):(t - length(SI) + 1)]) %*% head(SI, -1)
        d$lambda2[t] <- t(perc_q * d$mu_hat[(t - 1):(t - length(SI) + 1)]) %*% head(SI, -1)
        d$mu_hat[t] <- d$lambda1[t] * d$R[t] + d$R_q[t] * d$lambda2[t]
    }
    
    d
}



sim_dat <- R_draws %>% 
    bind_rows(future_R) %>% 
    expand_grid(perc_q = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) %>% 
    filter(iter %in% 1:1000) %>%
    group_by(iter, perc_q) %>% 
    mutate(lambda1 = lambda1[-1],
           lambda2 = lambda2[-1],
           R_q = m$R_q[iter],
           mu_hat = R * lambda1 + R_q * lambda2,
           perc_q2 = perc_q) %>% 
    group_modify(make_preds) %>% 
    ungroup %>% 
    mutate(new_cases = rnbinom(n(), mu = mu_hat, size = m$phi[iter])) %>% 
    group_by(iter, perc_q) %>% 
    mutate(total_cases = cumsum(new_cases))

plot_dat <- sim_dat %>% 
    mutate(date = ymd("2020-02-28") + day) %>%
    filter(date >= Sys.Date()) %>% 
    group_by(iter, perc_q) %>% 
    mutate(value = total_cases - min(total_cases)) %>% 
    filter(date == max(date)) %>% 
    group_by(perc_q) %>% 
    summarise(lower_50 = quantile(value, 0.25),
              upper_50 = quantile(value, 0.75),
              lower_60 = quantile(value, 0.2),
              upper_60 = quantile(value, 0.8),
              lower_70 = quantile(value, 0.15),
              upper_70 = quantile(value, 0.85),
              lower_80 = quantile(value, 0.1),
              upper_80 = quantile(value, 0.9),
              lower_90 = quantile(value, 0.05),
              upper_90 = quantile(value, 0.95),
              lower_95 = quantile(value, 0.025),
              upper_95 = quantile(value, 0.975)) %>% 
    pivot_longer(c(-perc_q), names_to = c("which", "prob"), names_sep = "_") %>% 
    pivot_wider(names_from = which, values_from = value) %>% 
    mutate(prob = parse_number(prob))

plot_dat %>% 
    ggplot(aes(perc_q, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    scale_fill_brewer() +
    scale_y_log10()  +
    scale_x_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = 0)) +
    labs(x = "Hlutfall í sóttkví",
         y = "Heildarfjöldi greindr smita (LOG skali)",
         title = "Heildarfjöldi smitaðra milli 4. og 31. október",
         subtitle = "Sýnt sem fall af % í sóttkví við greiningu") +
    theme(plot.margin = margin(5, 15, 5, 5)) +
    ggsave("sottkvi_smit_log.png", device = "png",
           width = 5, height = 0.5 * 5, scale = 2)

plot_dat %>% 
    ggplot(aes(perc_q, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    scale_fill_brewer() +
    scale_x_continuous(labels = label_percent(), limits = c(0, 1), expand = expansion(mult = 0)) +
    labs(x = "Hlutfall í sóttkví",
         y = "Heildarfjöldi greindr smita (LOG skali)",
         title = "Heildarfjöldi smitaðra milli 4. og 31. október",
         subtitle = "Sýnt sem fall af % í sóttkví við greiningu") +
    theme(plot.margin = margin(5, 15, 5, 5)) +
    ggsave("sottkvi_smit.png", device = "png",
           width = 5, height = 0.5 * 5, scale = 2)


#### Sviðsmyndir ####
    
plot_dat <- sim_dat %>% 
    filter(perc_q == 0.75) %>% 
    pivot_longer(c(-iter, -day, -lambda1, -lambda2, -mu_hat)) %>% 
    group_by(day, name) %>% 
    summarise(lower_50 = quantile(value, 0.25),
              upper_50 = quantile(value, 0.75),
              lower_60 = quantile(value, 0.2),
              upper_60 = quantile(value, 0.8),
              lower_70 = quantile(value, 0.15),
              upper_70 = quantile(value, 0.85),
              lower_80 = quantile(value, 0.1),
              upper_80 = quantile(value, 0.9),
              lower_90 = quantile(value, 0.05),
              upper_90 = quantile(value, 0.95),
              lower_95 = quantile(value, 0.025),
              upper_95 = quantile(value, 0.975)) %>% 
    pivot_longer(c(-day, -name), names_to = c("which", "prob"), names_sep = "_") %>% 
    pivot_wider(names_from = which, values_from = value) %>% 
    mutate(prob = parse_number(prob),
           date = ymd("2020-02-28") + day)

p5 <- plot_dat %>% 
    filter(name == "new_cases") %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_point(data = d %>% rename(new_cases = local) %>% pivot_longer(c(new_cases)),
               inherit.aes = F, aes(x = date, y = value), size = 0.8) +
    geom_vline(xintercept = Sys.Date(), lty = 2) +
    scale_x_date(date_breaks = "month", 
                 date_labels = "%B %d",
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1 + pred_days), 
                 expand = expansion(add = 0)) +
    scale_y_continuous(expand = expansion(mult = 0)) +
    scale_fill_brewer() +
    labs(subtitle = "New local cases") +
    theme(axis.title = element_blank()) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(5, 25, 5, 15),
          legend.title = element_blank()) 

p6 <- plot_dat %>% 
    filter(name == "total_cases") %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_point(data = d %>% mutate(total_cases = cumsum(local)) %>% pivot_longer(c(total_cases)),
               inherit.aes = F, aes(x = date, y = value), size = 0.8) +
    geom_vline(xintercept = Sys.Date(), lty = 2) +
    scale_x_date(date_breaks = "month", 
                 date_labels = "%B %d",
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1 + pred_days), 
                 expand = expansion(add = 0)) +
    scale_y_continuous(expand = expansion(mult = 0)) +
    scale_fill_brewer() +
    labs(subtitle = "Total local cases") +
    theme(axis.title = element_blank()) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = margin(5, 25, 5, 5),
          legend.title = element_blank()) 

p7 <- plot_dat %>% 
    filter(name == "R") %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_vline(xintercept = Sys.Date(), lty = 2) +
    scale_x_date(date_breaks = "month", date_labels = "%B %d",
                 limits = c(ymd("2020-02-27"), Sys.Date() + 1 + pred_days), 
                 expand = expansion(add = 0)) +
    scale_y_continuous(expand = expansion(mult = 0), breaks = pretty_breaks(8)) +
    scale_fill_brewer() +
    ggtitle(label = waiver(),
            subtitle = latex2exp::TeX("$R_t$")) +
    theme(axis.title = element_blank(),
          plot.margin = margin(5, 25, 5, 25))

plot_grid(p5, p6, p7, ncol = 1) +
    ggsave("svidsmynd.png", device = "png",
           width = 5, height = 5, scale = 2)





