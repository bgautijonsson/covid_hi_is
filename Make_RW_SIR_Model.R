library(tidyverse)
library(rstan)
library(lubridate)
library(readxl)
library(here)
library(cmdstanr)

options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")

Make_RW_SIR_Model <- function(fit_date = Sys.Date(), warmup = 500, iters = 500, chains = 4) {
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>%
        rename(new_cases = Innanlands_Smit, total_cases = Smit_Samtals, active_cases = Virk_Smit, new_recovered = Batnað) %>% 
        mutate(N = 364220,
               S = N - total_cases,
               S = lag(S, 1, default = 0),
               active_cases = lag(active_cases, 1, default = 0),
               dI_dt = new_cases,
               date = ymd(Dagsetning)) %>% 
        select(date, dI_dt, dR_dt = new_recovered, S, I = active_cases) %>% 
        filter(date >= ymd("2020-02-29"), date <= fit_date)
    
    N_days <- nrow(d)
    
    S <- d$S
    I <- d$I
    dI_dt <- d$dI_dt
    dR_dt <- d$dR_dt
    N <- 364220
    
    days <- seq_len(N_days) - 1
    
    stan_data <- list(
        N_days = N_days,
        N = N,
        S  = S,
        I = I,
        dI_dt = dI_dt,
        dR_dt = dR_dt
    )
    
    mod <- cmdstan_model(here("Stan", "RW_SIR.stan"))
    
    fit <- mod$sample(
        data = stan_data, 
        show_messages = FALSE, 
        chains = chains, 
        parallel_chains = chains,
        iter_sampling = iters,
        iter_warmup = warmup,
        max_treedepth = 15,
        init = 0,
        refresh = 100
    )
    
    fit$save_object(file = here("Results", "Models", "RW SIR", str_c("Model_", fit_date, ".rds")))
    
}


