library(shiny)
library(gridExtra)

source('../Scenario_EpiEstim.R')

future_R <- function(R_t,t) {
    R_t/(log(t+20)-2)
}

#Setting up the Shiny Server
shinyServer(function(input, output, session) {
    prepare_data <- reactive({
        date <- as.Date('2020-09-09')
        mod <- read_rds(here("Results", "Models", "EpiEstim", str_c("Model_",date, if_else(input$quarantine,'_w_quarantine',''),".rds")))
        m <- mod$draws() %>% as_draws_df
        d <- read_csv("https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA/export?format=csv&gid=1788393542",col_types=cols()) %>%
            select(date = Dagsetning, local = Innanlands_Smit,border_1=Landamaeri_Smit_1,border_2=Landamaeri_Smit_2,
                   imported = Innflutt_Smit,prop_quarantine=Hlutf_Sottkvi, num_quarantine=Fjoldi_Sottkvi) %>% 
            mutate(date = ymd(date),
                   total = local + imported,
                   prop_quarantine_and_border=if_else(input$quarantine & total!=0,(num_quarantine+border_1+border_2)/total,0)) %>% 
            filter(date >= ymd("2020-02-28"))
        SI <- get_SI_vec(nrow(d))
        d <- mutate(d,lambda=calculate_lambda(total,SI,prop_quarantine))
        R_draws <- spread_draws(m, R[day]) %>% 
                    group_by(day) %>% 
                    mutate(iter = row_number()) %>%
                    ungroup %>% 
                    select(iter, day, R)
        return(list('R_draws'=R_draws,'d'=d))
    }) 
    
    get_scenario_dat<-eventReactive(input$go,{
        theme_set(theme_classic(base_size = 12) + 
                      theme(legend.position = "none"))
        dat_list <- prepare_data()
        
        prop_extra = 0
        
        if(input$test == "first") prop_extra = 0.000812513
        else if(input$test == 'none') prop_extra = 0.001290147
        else if(input$test == 'second_ice') prop_extra = 0.000510378
        
        future_prop_quarantine = rep(0,pred_days-1)
        if(input$quarantine) {
            for(i in 1:(pred_days-1)) {
                future_prop_quarantine[i] = 1/(1+exp(5-i*10/42))
            }
        }
        
        plot_dat <- scenario(d=dat_list$d, R_draws=dat_list$R_draws, R_fun = future_R, future_prop_quarantine=future_prop_quarantine, prop_extra=prop_extra,pred_days=42,use_quarantine=input$quarantine)
        
        p5 <- plot_dat %>% 
            filter(name == "y_hat") %>% 
            ggplot(aes(date, ymin = lower, ymax = upper)) +
            geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
            geom_point(data = d %>% rename(y_hat = local) %>% pivot_longer(c(y_hat)),
                       inherit.aes = F, aes(x = date, y = value)) +
            geom_vline(xintercept = Sys.Date(), lty = 2) +
            scale_x_date(date_breaks = "month", 
                         date_labels = "%B %d",
                         limits = c(ymd("2020-02-27"), Sys.Date() + 1 + pred_days), 
                         expand = expansion(add = 0)) +
            scale_y_continuous(expand = expansion(mult = 0.01)) +
            scale_fill_brewer() +
            labs(subtitle = "New local cases") +
            theme(axis.title = element_blank(),
                  plot.margin = margin(5, 5, 5, 5),
                  legend.title = element_blank()) 
        
        p6 <- plot_dat %>% 
            filter(name == "R") %>% 
            ggplot(aes(date, ymin = lower, ymax = upper)) +
            geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
            geom_hline(yintercept = 1, lty = 2) +
            geom_vline(xintercept = Sys.Date(), lty = 2) +
            scale_x_date(date_breaks = "month", 
                         date_labels = "%B %d",
                         limits = c(ymd("2020-02-27"), Sys.Date() + 1 + pred_days), 
                         expand = expansion(add = 0)) +
            scale_y_continuous(expand = expansion(mult = 0.01), breaks = pretty_breaks(8)) +
            scale_fill_brewer() +
            ggtitle(label = waiver(),
                    subtitle = latex2exp::TeX("$R_t$")) +
            theme(axis.title = element_blank(),
                  plot.margin = margin(5, 5, 5, 8))
        
        median_plot <- plot_dat %>% 
            filter(name == "y_hat", date>=Sys.Date(), prob==50) %>% 
            ggplot(aes(date, (lower+upper)/2)) +
            geom_line(col = "#0065ad", alpha = 0.7) +
            geom_vline(xintercept = Sys.Date(), lty = 2) +
            scale_x_date(date_breaks = 'day', 
                         date_labels = "%B %d",
                         limits = c(Sys.Date(), Sys.Date() + 1 + pred_days), 
                         expand = expansion(add = 0)) +
            scale_y_continuous(expand = expansion(mult = 0.01)) +
            labs(subtitle = "New local cases") +
            theme(axis.title = element_blank(),
                  plot.margin = margin(5, 5, 5, 5),
                  legend.title = element_blank()) 
        
        plot_grid(p5, p6, median_plot, ncol = 1)
    }) 
    
    output$epidemia_plot <- renderPlot({
        p = get_scenario_dat()
        if (is.null(p))
            return(NULL)
        p        
    })

    output$downloadPlot <- downloadHandler(
        filename<-function(){
            paste(gsub(" ","-",input$title),"-",format(Sys.Date(), "%b-%d-%Y"), ".pdf", sep="")
        },
        content=function(file=NULL) {
            pdf(file,width=9,height=6)
            if (is.null(epidemia_plot())) return(NULL)
            epi_estim_plot(quar=input$quarantine, first_test=input$first_test, second_test=input$second_test)
            dev.off()
        }
    )
    
    output$downloadCSV <- downloadHandler(
        filename <- function() {
            return('ModelEstimateTable')
        },
        content <- function(file=NULL) {
            csv(file)
            dev.off()
        }
    )
})