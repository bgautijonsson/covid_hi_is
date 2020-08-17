library(readr)
library(dplyr)
library(rstan)
library(magrittr)
library(stringr)
library(lubridate)
library(here)
library(cmdstanr)
options(mc.cores = parallel::detectCores())
parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
source("Make_Stan_Data.R")

Make_IGLGM_Model <- function(fit_date = Sys.Date(), warmup = 500, iters = 500, chains = 4, threads_per_chain = 1) {
    
    
    
    
    
    stan_data <- Make_Stan_Data(stop_date = fit_date) 
    
    
    mod <- cmdstan_model(here("Stan", "Hierarchical_IGLGM.stan"),
                         cpp_options = list(stan_threads = TRUE))
    
    
    fit <- mod$sample(
        data = stan_data, 
        show_messages = FALSE, 
        chains = chains, 
        parallel_chains = chains,
        threads_per_chain = threads_per_chain,
        iter_sampling = iters,
        iter_warmup = warmup,
        max_treedepth = 15,
        init = 0,
        refresh = 100
    )
    
    fit$save_object(file = here("Results", "Models", "IGLGM", str_c("Model_", fit_date, ".rds")))
    
    
}