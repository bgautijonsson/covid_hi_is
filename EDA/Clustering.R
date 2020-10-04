logit <- function(x) log(x / (1 - x))
dists <- spread_draws(m, 
                      alpha[wave_id], beta[wave_id], S[wave_id], nu[wave_id],
                      mu_alpha, sigma_alpha,
                      mu_beta, sigma_beta,
                      mu_S, sigma_S,
                      mu_nu, sigma_nu) %>% 
    mutate(
        max_incidence = S * (1 - (1 + nu)^(-1/nu)),
        top = alpha + 1/beta * log(1/(0.1)^nu - 1) - log(nu),
        alpha = (log(alpha) - mu_alpha) / sigma_alpha,
        beta = (log(beta) - mu_beta) / sigma_beta,
        S = (logit(S) - mu_S) / sigma_S,
        nu = (log(nu) - mu_nu) / sigma_nu
    ) %>% 
    filter(top > 0, is.finite(top)) %>% 
    group_by(.chain, .iteration, .draw) %>% 
    mutate(max_incidence = scale(logit(max_incidence)),
           top = scale(log(top))) %>% 
    select(-starts_with("mu"), -starts_with("sigma")) %>% 
    group_by(wave_id) %>% 
    summarise(
        max_incidence = mean(max_incidence),
        top = mean(top),
        alpha = mean(alpha),
        S = mean(S),
        nu = mean(nu),
        beta = mean(beta)
    ) %>% 
    select(-wave_id) %>% 
    prcomp %>% 
    augment %>% 
    select(-.rownames) %>% 
    dist %>% 
    tidy %>% 
    rename(wave_id1 = item1, wave_id2 = item2) %>% 
    mutate_at(vars(1, 2), ~as.numeric(as.character(.))) %>% 
    inner_join(model_d %>% 
                   distinct(location, wave_id) %>% 
                   group_by(location) %>% 
                   mutate(which_wave1 = wave_id - min(wave_id) + 1) %>% 
                   rename(location1 = location),
               by = c("wave_id1" = "wave_id")) %>% 
    inner_join(model_d %>% 
                   distinct(location, wave_id) %>% 
                   group_by(location) %>% 
                   mutate(which_wave2 = wave_id - min(wave_id) + 1) %>% 
                   rename(location2 = location),
               by = c("wave_id2" = "wave_id"))

waves <- dists %>% 
    filter(location1 == "Iceland", which_wave1 == 1) %>% 
    arrange(distance) %>% 
    select(-location1, -which_wave1, -wave_id1) %>% 
    rename(location = location2, which_wave = which_wave2) %>% 
    select(location, which_wave, wave_id2, distance) %>% 
    top_n(5, -distance) %>% 
    add_row(location = "Iceland", wave_id2 = 57, which_wave = 1, distance = 0)

par_res %>% 
    filter(wave_id %in% c(waves$wave_id2)) %>% 
    expand_grid(t = seq(0, 100)) %>% 
    mutate(z = beta * (t - alpha),
           f = S - S / (1 + nu * exp(z))^(1/nu),
           dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu) %>% 
    pivot_longer(c(f, dfdt)) %>% 
    group_by(wave_id, t, name) %>% 
    summarise(median = median(value)) %>% 
    inner_join(waves, by = c("wave_id" = "wave_id2")) %>% 
    mutate(location = paste0(location, " (", which_wave, ")"),
           name = fct_recode(name,
                             "Dagleg greind smit" = "dfdt",
                             "Uppsöfnuð greind smit" = "f")) %>% 
    ungroup %>% 
    mutate(location = fct_reorder(location, distance)) %>% 
    ggplot(aes(t, median * 100000, group = location, col = location)) +
    geom_line(aes(size = str_detect(location, "Iceland")), alpha = 1) +
    scale_x_continuous(limits = c(0, 100), expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = 0.01), limits = c(0, NA)) +
    # scale_colour_brewer(type = "qual", palette = "Set1", 
    #                     guide = guide_legend(override.aes = list(size = 1.1))) +
    scale_colour_manual(values = c("#377eb8", "#e41a1c", "#4daf4a", "#984ea3", "#a65628", "#ff7f00"),
                        guide = guide_legend(override.aes = list(size = 1.2))) +
    scale_size_manual(values = c(0.6, 1.4), guide = NULL) +
    labs(col = "",
         y = "Tíðni á 100.000 íbúa",
         x = "Dagar frá upphafi bylgju") +
    theme(legend.position = c(0.9, 0.75), 
          legend.background = element_rect(colour = "black"), 
          legend.title = element_blank(),
          panel.spacing = unit(0.5, "cm")) +
    facet_grid(name ~ ., scales = "free_y") +
    ggsave("Clustering1.png", width = 5, height = 0.621 * 5, scale = 2.5)

waves <- dists %>% 
    filter(location1 == "Iceland", which_wave1 == 3) %>% 
    arrange(distance) %>% 
    select(-location1, -which_wave1, -wave_id1) %>% 
    rename(location = location2, which_wave = which_wave2) %>% 
    select(location, which_wave, wave_id2, distance) %>% 
    top_n(5, -distance) %>% 
    add_row(location = "Iceland", wave_id2 = 59, which_wave = 3, distance = 0)

par_res %>% 
    filter(wave_id %in% c(waves$wave_id2)) %>% 
    expand_grid(t = seq(0, 100)) %>% 
    mutate(z = beta * (t - alpha),
           f = S - S / (1 + nu * exp(z))^(1/nu),
           dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu) %>% 
    pivot_longer(c(f, dfdt)) %>% 
    group_by(wave_id, t, name) %>% 
    summarise(median = median(value)) %>% 
    inner_join(waves, by = c("wave_id" = "wave_id2")) %>% 
    mutate(which_wave = case_when(location == "South Korea" ~ 2,
                                  location == "Iceland" ~ 2,
                                  TRUE ~ which_wave),
           location = paste0(location, " (", which_wave, ")"),
           name = fct_recode(name,
                             "Dagleg greind smit" = "dfdt",
                             "Uppsöfnuð greind smit" = "f")) %>% 
    ungroup %>% 
    mutate(location = fct_reorder(location, distance)) %>% 
    ggplot(aes(t, median * 100000, group = location, col = location)) +
    geom_line(aes(size = str_detect(location, "Iceland")), alpha = 0.9) +
    scale_x_continuous(limits = c(0, 100), expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = 0.01), limits = c(0, NA)) +
    # scale_colour_brewer(type = "qual", palette = "Set1", 
    #                     guide = guide_legend(override.aes = list(size = 1.1))) +
    scale_colour_manual(values = c("#377eb8", "#a65628", "#984ea3", "#ff7f00", "#4daf4a", "#e41a1c"),
                        guide = guide_legend(override.aes = list(size = 1.2))) +
    scale_size_manual(values = c(0.6, 1.4), guide = NULL) +
    labs(y = "Tíðni á 100.000 íbúa",
         x = "Dagar frá upphafi bygju") +
    theme(legend.position = c(0.9, 0.75), 
          legend.background = element_rect(colour = "black"), 
          legend.title = element_blank(),
          panel.spacing = unit(0.5, "cm")) +
    facet_grid(name ~ ., scales = "free_y") +
    ggsave("Clustering2.png", width = 5, height = 0.621 * 5, scale = 2.5)




waves <- dists %>% 
    filter(location1 == "Iceland", which_wave1 == 4) %>% 
    arrange(distance) %>% 
    select(-location1, -which_wave1, -wave_id1) %>% 
    rename(location = location2, which_wave = which_wave2) %>% 
    select(location, which_wave, wave_id2, distance) %>% 
    top_n(5, -distance) %>% 
    add_row(location = "Iceland", wave_id2 = 60, which_wave = 3, distance = 0)

par_res %>% 
    filter(wave_id %in% c(waves$wave_id2)) %>% 
    expand_grid(t = seq(0, 100)) %>% 
    mutate(z = beta * (t - alpha),
           f = S - S / (1 + nu * exp(z))^(1/nu),
           dfdt = beta * (S - f) * (1 - ((S - f) / S)^nu) / nu) %>% 
    pivot_longer(c(f, dfdt)) %>% 
    group_by(wave_id, t, name) %>% 
    summarise(median = median(value)) %>% 
    inner_join(waves, by = c("wave_id" = "wave_id2")) %>% 
    mutate(which_wave = case_when(location == "South Korea" ~ 2,
                                  location == "Iceland" ~ 2,
                                  TRUE ~ which_wave),
           location = paste0(location, " (", which_wave, ")"),
           name = fct_recode(name,
                             "Dagleg greind smit" = "dfdt",
                             "Uppsöfnuð greind smit" = "f")) %>% 
    ungroup %>% 
    mutate(location = fct_reorder(location, distance)) %>% 
    ggplot(aes(t, median * 100000, group = location, col = location)) +
    geom_line(aes(size = str_detect(location, "Iceland")), alpha = 0.9) +
    scale_x_continuous(limits = c(0, 100), expand = expansion(mult = 0)) +
    scale_y_continuous(expand = expansion(mult = 0.01), limits = c(0, NA)) +
    # scale_colour_brewer(type = "qual", palette = "Set1", 
    #                     guide = guide_legend(override.aes = list(size = 1.1))) +
    scale_colour_manual(values = c("#377eb8", "#a65628", "#984ea3", "#ff7f00", "#4daf4a", "#e41a1c"),
                        guide = guide_legend(override.aes = list(size = 1.2))) +
    scale_size_manual(values = c(0.6, 1.4), guide = NULL) +
    labs(y = "Tíðni á 100.000 íbúa",
         x = "Dagar frá upphafi bygju") +
    theme(legend.position = c(0.9, 0.75), 
          legend.background = element_rect(colour = "black"), 
          legend.title = element_blank(),
          panel.spacing = unit(0.5, "cm")) +
    facet_grid(name ~ ., scales = "free_y") +
    ggsave("Clustering2.png", width = 5, height = 0.621 * 5, scale = 2.5)
