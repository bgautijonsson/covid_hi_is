library(shiny)
library(gridExtra)

source('../Scenario_EpiEstim.R')

future_R <- function(R_t,t) {
    R_t/(log(t+9)-1)
}

#Setting up the Shiny Server
shinyServer(function(input, output, session) {
    epidemia_plot5<-eventReactive(input$go,{ 
        if(input$test == "first") prop_extra = 3/4500
        else if(input$test == 'none') prop_extra = 15/4500
        else if(input$test == 'second_ice') prop_extra = 2/4500
        
        plot_dat <- scenario(date=as.Date('2020-09-09'), R_fun = future_R,prop_extra=prop_extra,pred_days=42,use_quarantine=input$quarantine)
        
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
            theme(axis.title = element_blank()) +
            theme(axis.title = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  plot.margin = margin(5, 5, 5, 5),
                  legend.title = element_blank()) 
        
        return(p5)
    }) 
    
    output$epidemia_plot <- renderPlot({
        p = epidemia_plot5()
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