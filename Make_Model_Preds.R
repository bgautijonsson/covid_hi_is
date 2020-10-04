library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(tidybayes)
library(scales)
library(lubridate)
library(cmdstanr)
library(posterior)
library(purrr)
library(here)

Make_Model_Preds <- function(fit_date = Sys.Date()) {
    d <- read_csv(here("Results", "Data", str_c("COVID_Data_", fit_date, ".csv")))
    
    mod <- read_rds(here("Results", "Models", "IGLGM", str_c("Model_", fit_date, ".rds")))
    
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
    
    get_preds <- function(loc, pred_date = Sys.Date() + years(1)) {
        
        writeLines(str_c("Predicting for country ", which(locs == loc),
                         "/", length(locs), " : ", loc))
        
        plot_dat <- d %>% filter(location == loc)
        info_dat <- d %>% filter(location == loc)
        
        location_id <- unique(info_dat$location_id)
        wave_id <- unique(info_dat$wave_id)
        pop <- unique(info_dat$population)
        start_cases <- min(info_dat$total_cases)
        
        start_date <- min(info_dat$date)
        
        end_date <- max(info_dat$date)
        
        
        days_in_data <- max(info_dat$days) + 1
        
        
        day_seq <- seq(0, as.numeric(pred_date - start_date), by = 1)
        
        
        join_dat <- info_dat %>% 
            group_by(location_id, wave_id) %>% 
            summarise(start_day = min(days),
                      end_day = max(days), .groups = "drop") %>% 
            ungroup %>% 
            mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
                   end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
        
        
        
        mean_results_total <-  par_res %>% 
            inner_join(join_dat, by = "wave_id") %>% 
            expand_grid(days = day_seq) %>% 
            filter(days >= start_day, days < end_day) %>% 
            mutate(z = beta * (days - alpha - start_day),
                   f = S - S / (1 + nu * exp(z))^(1/nu),
                   dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
                   new_cases = dfdt * pop) %>% 
            group_by(iter) %>% 
            mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
            ungroup %>% 
            mutate(type = "mean", waves = "total",
                   date = min(plot_dat$date) + days) %>% 
            select(iter, days, date, new_cases, total_cases, type, waves)
        
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
        
        plot_dat <- plot_dat %>% filter(wave_id == max(wave_id))
        info_dat <- info_dat %>% filter(wave_id == max(wave_id))
        
        location_id <- unique(info_dat$location_id)
        wave_id <- unique(info_dat$wave_id)
        pop <- unique(info_dat$population)
        start_cases <- min(info_dat$total_cases)
        
        start_date <- min(info_dat$date)
        
        end_date <- max(info_dat$date)
        
        
        days_in_data <- max(info_dat$days) + 1
        
        
        day_seq <- seq(0, as.numeric(pred_date - start_date), by = 1)
        
        join_dat <- info_dat %>% 
            group_by(location_id, wave_id) %>% 
            summarise(start_day = min(days),
                      end_day = max(days), .groups = "drop") %>% 
            ungroup %>% 
            mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
                   end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
        
        mean_results_latest <-  par_res %>% 
            inner_join(join_dat, by = "wave_id") %>% 
            expand_grid(days = day_seq) %>% 
            filter(days >= start_day, days < end_day) %>% 
            mutate(z = beta * (days - alpha - start_day),
                   f = S - S / (1 + nu * exp(z))^(1/nu),
                   dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu,
                   new_cases = dfdt * pop) %>% 
            group_by(iter) %>% 
            mutate(total_cases = as.numeric(cumsum(new_cases * (days > 0))) + start_cases) %>% 
            ungroup %>% 
            mutate(type = "mean", waves = "latest",
                   date = min(plot_dat$date) + days) %>% 
            select(iter, days, date, new_cases, total_cases, type, waves)
        
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
        
        mean_results_total %>% 
            bind_rows(obs_results_total) %>% 
            bind_rows(mean_results_latest) %>% 
            bind_rows(obs_results_latest) %>% 
            pivot_longer(c(new_cases, total_cases)) %>% 
            group_by(days, name, type, waves, date) %>% 
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
            pivot_longer(c(-days, -name, -type, -waves,-date), names_to = c("which", "prob"), names_sep = "_") %>% 
            mutate(location = loc)
        
    }
    
    locs <- unique(d$location)
    results <- locs %>% 
        map_df(get_preds, pred_date = Sys.Date() + years(1))
    
    write_csv(results, here("Results", "Predictions", str_c("Predictions_", fit_date, ".csv")))
    write_rds(results, here("Results", "Predictions", str_c("Predictions_", fit_date, ".rds")))
    write_csv(results, here("Prediction_App", "shiny_preds.csv"))
    
}