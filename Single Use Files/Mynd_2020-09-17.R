model_d <- here("Results", "Data", str_c("COVID_Data_", Sys.Date(), ".csv")) %>% read_csv

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

plot_dat <- model_d %>% filter(location == "Iceland") %>% filter(wave_id == max(wave_id))
info_dat <- model_d %>% filter(location == "Iceland") %>% filter(wave_id == max(wave_id))

location_id <- unique(info_dat$location_id)
wave_id <- unique(info_dat$wave_id)
pop <- unique(info_dat$population)
start_cases <- min(info_dat$total_cases)

start_date <- min(info_dat$date)

end_date <- max(info_dat$date)


days_in_data <- max(info_dat$days) + 1


day_seq <- seq(0, as.numeric(Sys.Date() + weeks(3) - start_date), by = 1)


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
    geom_point(data = plot_dat, inherit.aes = F,
               aes(date, new_cases, 
                   col = date > ymd("2020-09-09"),
                   size = date > ymd("2020-09-09"))) +
    geom_vline(xintercept = ymd("2020-09-09"), lty = 2) +
    scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0), limits = c(0, 20)) +
    scale_x_date(breaks = plot_breaks, labels = icelandic_dates, expand = expansion(add = 0)) +
    scale_colour_manual(values = c("black", "#d6604d")) +
    scale_size_manual(values = c(1.4, 2)) +
    scale_fill_brewer() +
    labs(subtitle = "Innlend dagleg smit") +
    annotation_custom(grob = logo, 
                      xmin = ymd("2020-09-20"), xmax = Sys.Date() + weeks(3),
                      ymin = 12, ymax = 18) +
    theme(axis.title = element_blank(), 
          plot.margin = margin(5, 5, 5, 10))


p2 <- pred_dat %>% 
    filter(name == "total_cases") %>% 
    ggplot(aes(date, ymin = lower, ymax = upper)) +
    geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
    geom_point(data = plot_dat, inherit.aes = F, size = 1.6,
               aes(date, total_cases, 
                   col = date > ymd("2020-09-09"),
                   size = date > ymd("2020-09-09"))) +
    geom_vline(xintercept = ymd("2020-09-09"), lty = 2) +
    scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0), limits = c(0, 400)) +
    scale_x_date(breaks = plot_breaks, labels = icelandic_dates, expand = expansion(add = 0)) +
    scale_colour_manual(values = c("black", "#d6604d")) +
    scale_size_manual(values = c(1.4, 2)) +
    scale_fill_brewer() +
    labs(subtitle = "Innlend uppsöfnuð smit") +
    theme(axis.title = element_blank(), 
          plot.margin = margin(5, 5, 5, 5))


plot_grid(p1, p2, ncol = 1) +
    ggsave(here("Results", "Figures", "latest_spa.png"), device = "png", 
           height = 0.621 * 5, width = 5, scale = 2)
