#### Help functions ####
make_preds <- function(d, ...) {
    for (t in seq(N_days, nrow(d))) {
        d$lambda[t] <- t(d$mu_hat[(t - 1):(t - length(SI) + 1)]) %*% head(SI, -1)
        d$mu_hat[t] <- d$lambda[t] * d$R[t]
    }
    return(d)
}

get_SI_vec <- function(N_days,shape=1.54,rate=0.28){
    SI_dat <- tibble(t = seq(1, N_days)) %>% 
        mutate(
            p = case_when(
                TRUE ~ pgamma(t + 0.5, shape = shape, rate = rate) - pgamma(t - 0.5, shape = shape, rate = rate)
            ),
            p = p / sum(p)
        )
    return(SI_dat$p)
}

calculate_labmda <- function(total,SI){
    N_days <- length(total)
    lambda <- numeric(N_days)
    for (t in 2:N_days) {
        lambda[t] <- t(head(d$total, t - 1)) %*% tail(rev(SI), t - 1) / sum(tail(rev(SI), t - 1))
    }
    return(lambda)
}


scenario <- function(date=Sys.Date(),R_fun,prop_extra,num_border_tests=2000,pred_days=42,use_quarantine=T){
    mod <- read_rds(here("Results", "Models", "EpiEstim", str_c("Model_",date, if_else(use_quarantine,'_w_quarantine',''),".rds")))
    m <- mod$draws() %>% as_draws_df
    d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542",col_types=cols()) %>%
        select(date = Dagsetning, local = Innanlands_Smit, imported = Innflutt_Smit) %>% 
        mutate(date = ymd(date),
               total = local + imported) %>% 
        filter(date >= ymd("2020-02-28"))
    SI <- get_SI_vec(nrow(d))
    lambda <- calculate_labmda(d$total,SI)
    lambda <- c(lambda,rep(0,pred_days))
    R_draws <- spread_draws(m, R[day]) %>% 
                group_by(day) %>% 
                mutate(iter = row_number()) %>%
                ungroup %>% 
                select(iter, day, R)
    
    N_iter <- 2000
    last_R <- R_draws %>% 
        filter(day == max(day)) %>% 
        .$R
    future_R <- crossing(day = max(R_draws$day) + seq_len(pred_days),
                         iter = seq_len(N_iter)) %>% 
                    group_by(iter) %>% 
                    mutate(R = R_fun(last_R[iter],day-N_days)) %>%
                           #R = as.numeric(R) + cumsum(rnorm(n(), sd = 0.02))) %>% 
                    ungroup()
    
    plot_dat <- R_draws %>% 
        bind_rows(future_R) %>% 
        group_by(iter) %>% 
        mutate(lambda = lambda[-1],
               mu_hat = R * lambda) %>% 
        # filter(iter %in% 1:100) %>% 
        group_by(iter) %>% 
        group_modify(make_preds) %>% 
        ungroup %>% 
        mutate(y_hat = rnbinom(n(), mu = mu_hat+if_else(day>N_days,as.numeric(rbinom(n(),size=num_border_tests,prob = prop_extra)),0), size = 6)) %>% 
        pivot_longer(c(-iter, -day, -lambda, -mu_hat)) %>% 
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
    return(plot_dat)
}
