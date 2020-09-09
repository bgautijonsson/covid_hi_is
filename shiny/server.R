library(shiny)
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

future_R <- function(R_t) {
    1*R_t
}

#Setting up the Shiny Server
shinyServer(function(input, output, session) {
    epidemia_plot<-eventReactive(input$go,{ 
        if(input$test == "first") prop_extra = 3/4500
        else if(input$test == 'none') prop_extra = 15/4500
        else if(input$test == 'second_ice') prop_extra = 2/4500
        
        plot_dat <- scenario(R_fun = future_R,prop_extra=prop_extra,pred_days=42,use_quarantine=input$quarantine)
        
        p6 <- plot_dat %>% 
            filter(name == "R") %>% 
            ggplot(aes(date, ymin = lower, ymax = upper)) +
            geom_ribbon(aes(fill = factor(-prob)), alpha = 0.7) +
            geom_hline(yintercept = 1, lty = 2) +
            geom_vline(xintercept = Sys.Date(), lty = 2) +
            scale_x_date(date_breaks = "month", date_labels = "%B %d",
                         limits = c(ymd("2020-02-27"), Sys.Date() + 1 + pred_days), 
                         expand = expansion(add = 0)) +
            scale_y_continuous(expand = expansion(mult = 0.01), breaks = pretty_breaks(8)) +
            scale_fill_brewer() +
            ggtitle(label = waiver(),
                    subtitle = latex2exp::TeX("$R_t$")) +
            theme(axis.title = element_blank(),
                  plot.margin = margin(5, 5, 5, 8))
        
        p6
    })   
    
    output$epidemia_plot <- renderPlot({
        if (is.null(epidemia_plot()))
            return(NULL)
        
        epidemia_plot()
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