Make_Stan_Data <- function(min_case_rate = 0.02, 
                           countries = NULL, 
                           stop_date = Sys.Date()) {
    options(tidyverse.quiet = TRUE)
    
    library(tidyverse, verbose = F, warn.conflicts = F, attach.required = T)
    library(readxl, verbose = F, warn.conflicts = F, attach.required = T)
    library(lubridate, verbose = F, warn.conflicts = F, attach.required = T)
    suppressPackageStartupMessages(library(mgcv, verbose = F, warn.conflicts = F, attach.required = T))
    library(broom, verbose = F, warn.conflicts = F, attach.required = T)
    library(here, verbose = F, warn.conflicts = F, attach.required = T)
    
    col_types <- cols(
        .default = col_double(),
        iso_code = col_character(),
        continent = col_character(),
        location = col_character(),
        date = col_date(),
        total_tests = col_number(),
        new_tests = col_number(),
        tests_units = col_character()
    )
    
    countries <- c(
        "Afghanistan", "Algeria",
        "Argentina", 
        "Armenia",
        "Australia", 
        "Austria", "Azerbaijan",
        "Bahrain", "Bangladesh", "Belarus", "Belgium", "Bolivia", "Brazil",
        "Canada", "Chile", "Colombia", "Costa Rica", "Croatia", "Czech Republic",
        "Denmark", "Dominican Republic",
        "Egypt", "El Salvador", "Ethiopia",
        "Finland", "France",
        "Germany", "Greece", "Guatemala",
        "Honduras",
        "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy",
        "Japan",
        "Kenya", "Kuwait",
        "Luxembourg",
        "Mexico", "Moldova", "Morocco",
        "Netherlands", "Nepal", "Nigeria", "Norway",
        "Oman",
        "Pakistan", "Panama", "Peru", "Philippines", "Poland", "Portugal",
        "Qatar",
        "Romania", "Russia",
        "Saudi Arabia", "Serbia", "Singapore", "Slovenia", "South Africa", "South Korea", "Spain", "Sweden", "Switzerland",
        "Turkey",
        "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uzbekistan",
        "Venezuela"
    )
    
    cut_dates <- tribble(
        ~"location", ~"cutoff",
        "Algeria", "2020-06-06",
        "Australia", "2020-05-02",
        "Austria", "2020-05-03",
        "Azerbaijan", "2020-04-27",
        "Belgium", "2020-06-15",
        "Canada", "2020-07-01",
        "Costa Rica", "2020-05-01",
        "Croatia", "2020-06-01",
        "Czech Republic", "2020-05-28",
        "Denmark", "2020-06-20",
        "Ethiopia", "2020-07-06",
        "Finland", "2020-06-20",
        "France", "2020-05-11",
        "Germany", "2020-06-01",
        "Greece", "2020-05-26",
        "Iceland", "2020-05-05",
        "Iran", "2020-05-01",
        "Ireland", "2020-06-20",
        "Israel", "2020-05-16",
        "Italy", "2020-07-01",
        "Japan", "2020-06-01",
        "Luxembourg", "2020-06-01",
        "Moldova", "2020-05-10",
        "Morocco", "2020-06-01",
        "Nepal", "2020-07-18",
        "Netherlands", "2020-07-07",
        "Norway", "2020-07-01",
        "Panama", "2020-05-20",
        "Peru", "2020-07-06",
        "Poland", "2020-07-03",
        "Portugal", "2020-05-20",
        "Romania", "2020-06-01",
        "Saudi Arabia", "2020-05-29",
        "Serbia", "2020-06-01",
        "Singapore", "2020-07-01",
        "Slovenia", "2020-05-25",
        "South Korea", "2020-05-01",
        "Spain", "2020-06-08",
        "Sweden", "2020-05-23",
        "Switzerland", "2020-05-20",
        "Turkey", "2020-06-01",
        "Ukraine", "2020-05-26",
        "United Kingdom", "2020-07-01",
        "United States", "2020-06-10",
        "Uzbekistan", "2020-05-01"
    ) %>% 
        mutate(cutoff = ymd(cutoff))
    
    cut_dates2 <- tribble(
        ~"location", ~"cutoff2",
        "Australia", "2020-06-10",
        "Greece", "2020-07-19",
        "Iceland", "2020-07-23",
        "Turkey", "2020-07-23",
        "Ukraine", "2020-07-14"
    ) %>% 
        mutate(cutoff2 = ymd(cutoff2))
    
    iceland_d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542") %>%
        mutate(country = "Iceland", location = "Iceland", population = 364220, continent = "Europe") %>% 
        rename(new_deaths = "Dauðsföll", new_cases = Innanlands_Smit, total_deaths = Dauðsföll_Samtals) %>% 
        mutate(date = ymd(Dagsetning),
               total_cases = cumsum(new_cases),
               population_density = 3.4, median_age = 37.3, gdp_per_capita = 46483, diabetes_prevalence = 5.31) %>% 
        select(country, location, continent, date, new_cases, total_cases, new_deaths, total_deaths,
               population, population_density, median_age, gdp_per_capita, diabetes_prevalence)
    
    d <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                  col_types = col_types) %>%
        select(country = location, location = location, continent, 
               date, 
               new_cases, total_cases, 
               new_deaths, total_deaths, 
               population, population_density, median_age, gdp_per_capita, diabetes_prevalence) %>% 
        filter(country %in% countries, country != "Iceland") %>% 
        bind_rows(iceland_d) %>% 
        arrange(country, location, date) %>% 
        group_by(location) %>% 
        mutate(rolling_new_cases = data.table::frollmean(pmax(new_cases, 0), n = 7, align = "right")) %>% 
        ungroup %>% 
        mutate(case_rate = total_cases / population * 1000) %>% 
        filter(case_rate >= min_case_rate) %>% 
        group_by(location) %>% 
        mutate(days = row_number() - 1) %>% 
        ungroup %>% 
        filter(new_cases >= 0)
    
    
    d <- d %>%  
        filter(date <= stop_date) %>% 
        left_join(cut_dates) %>% 
        left_join(cut_dates2) %>% 
        mutate_at(vars(contains("cutoff")), ~  case_when(is.na(.) ~ ymd("2030-01-01"), 
                                                         TRUE ~ .)) %>% 
        mutate(wave_id = paste(location, 1 * (date >= cutoff) + 1 * (date >= cutoff2)) %>% as.factor %>% as.numeric) %>% 
        group_by(wave_id) %>% 
        mutate(days = days - min(days)) %>% 
        ungroup %>% 
        mutate(location_id = as.numeric(as.factor(location)))
    
    N_obs <- nrow(d)
    N_locations <- max(d$location_id)
    N_waves <- max(d$wave_id)
    
    
    days <- d$days
    new_cases <- d$new_cases
    location <- d %>% 
        distinct(wave_id, location_id) %>% 
        arrange(wave_id) %>%
        .$location_id
    wave <- d$wave_id %>%
        as.integer
    
    log_population <- d %>% 
        distinct(wave_id, population) %>% 
        arrange(wave_id) %>%  
        .$population %>% log
    
    wave_start <- d %>% 
        mutate(id = row_number()) %>% 
        group_by(wave_id) %>% 
        summarise(wave_start = min(id)) %>% 
        .$wave_start %>% 
        as.integer
    
    wave_stop <- d %>% 
        mutate(id = row_number()) %>% 
        group_by(wave_id) %>% 
        summarise(wave_stop = max(id)) %>% 
        .$wave_stop %>% 
        as.integer
    
    wave_length <- d %>% 
        mutate(id = row_number()) %>% 
        group_by(wave_id) %>% 
        summarise(length = n()) %>% 
        .$length %>% 
        as.integer
    
    stan_data <- list(N_obs = N_obs,
                      N_locations = N_locations,
                      N_waves = N_waves,
                      days = days, 
                      new_cases = new_cases, 
                      location = location,
                      log_population = log_population,
                      wave_start = wave_start,
                      wave_stop = wave_stop,
                      wave_length = wave_length,
                      grainsize = 1)
    
    write_csv(d, here("Results", "Data", str_c("COVID_Data_", stop_date, ".csv")))
    
    return(stan_data)
}