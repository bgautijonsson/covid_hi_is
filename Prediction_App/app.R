#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(cowplot)
library(tidybayes)
library(scales)
library(lubridate)
library(plotly)
library(shiny)
library(shinythemes)
library(data.table)

languages <- list("English" = list(), "Icelandic" = list())

languages$English$x_axis <- function(x) {
    as.character(lubridate::month(x, label = TRUE))
}

languages$Icelandic$x_axis <- function(x) {
    months <- c("janúar", "febrúar", "mars", "apríl", "maí", "júní", 
                "júlí", "ágúst", "september", "október", "nóvember", "desember")
    
    months[lubridate::month(x)]
}

devices <- list("PNG" = "png",
                "JPEG" = "jpg",
                "PDF" = "pdf",
                "TIFF" = "tiff")

theme_set(theme_classic(base_size = 12) + 
              background_grid(color.major = "grey90", 
                              color.minor = "grey95", 
                              minor = "xy", major = "xy") +
              theme(legend.position = "none"))


d <- fread("shiny_COVID_Data.csv")

pred_d <- fread("shiny_preds.csv")

sidebar_info <-
    paste0(
        '<div align="middle" class="center">
       <img src="hi_hvs_horiz.png" width="100%"/>
       </div>'
    )

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "University of Iceland",
    theme = shinytheme(theme = "flatly"),
    
    tabPanel(title = "Interactive COVID-19 Predictions",
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "location",
                                 label = "Location",
                                 choices = unique(d$location), selected = "Iceland"),
                     dateRangeInput(inputId = "date_range",
                                    label = "Dates to Plot", 
                                    start = ymd("2020-03-21"), end = Sys.Date() + months(1),
                                    min = ymd("2020-02-28"), max = max(pred_d$date), 
                                    weekstart = 1, startview = "year",
                                    format = "yy/mm/dd", separator = " - "),
                     fluidRow(
                         column(
                             6,
                             selectInput(inputId = "wave",
                                         label = "Wave",
                                         choices = c("All", "Latest"), selected = "All"),
                         ),
                         column(
                             6,
                             selectInput(inputId = "type",
                                         label = "Error Type", 
                                         choices = c("Mean", "Observation"), 
                                         multiple = F,
                                         selected = "Observation"),
                         )
                     ),
                     fluidRow(
                         column(6,
                                selectInput(inputId = "quantile",
                                            label = "Intervals (%)", 
                                            choices = unique(pred_d$prob), 
                                            multiple = T,
                                            selected = unique(pred_d$prob))
                         ),
                         column(6,
                                selectInput(inputId = "obs_type",
                                            label = "Observation type",
                                            choices = c("Count", "Incidence per 100.000"),
                                            selected = "Count"))
                     ),
                     div(
                         actionButton(inputId = "gobutton", label = "Predict", width = "120px"), 
                         class = "center", align = "middle"
                     ),
                     HTML(sidebar_info),
                     width = 3
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotlyOutput("pred_plot", height = "600px"),
                     fluidRow(
                         column(2,
                                br(),
                                downloadButton("downloadData", "Save Plot", style = "width:100%")
                         ), 
                         column(3,
                                selectInput(inputId = "language",
                                            label = "Saved Plot Language",
                                            choices = c("English", "Icelandic"),
                                            selected = "English"),
                         ), 
                         column(3,
                                selectInput(inputId = "device",
                                            label = "Plot Device",
                                            choices = c("PNG", "JPEG", "PDF", "TIFF"),
                                            selected = "JPEG")
                         ), 
                         column(3,
                                numericInput(inputId = "dpi",
                                             label = "DPI", min = 72, max = 320, 
                                             value = 150, step = 1)
                         ), 
                         
                     )
                 )
             )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    pred_plot <-  eventReactive(input$gobutton, {
        
        plot_type <- list("Mean" = "mean", "Observation" = "obs")
        plot_waves<- list("All" = "total", "Latest" = "latest")
        
        plot_dat <- d %>% filter(location == input$location)
        plot_pred <- pred_d %>% filter(location == input$location, 
                                       type == plot_type[[input$type]],
                                       waves == plot_waves[[input$wave]])
        
        if (input$wave != "All") {
            n_waves <- max(plot_dat$wave_id) - min(plot_dat$wave_id) + 1
            plot_wave <- as.numeric(input$wave)
            plot_wave <- unique(plot_dat$wave_id)[n_waves]
            
            plot_dat <- plot_dat %>%
                filter(wave_id == plot_wave)
            
            plot_pred <- plot_pred %>%
                filter(date >= min(plot_dat$date)) %>%
                pivot_wider() %>% 
                mutate(total_cases = total_cases - min(plot_dat$total_cases),
                       total_cases = pmax(total_cases, 0)) %>% 
                pivot_longer(c(new_cases, total_cases))
            
            plot_dat <- plot_dat %>% 
                mutate(total_cases = total_cases - min(total_cases) + new_cases[1])
        }
        
        if (input$obs_type == "Incidence per 100.000") {
            plot_pred <- plot_pred %>% 
                mutate_at(vars(value), ~ . / unique(plot_dat$population) * 100000)
            
            plot_dat <- plot_dat %>% 
                mutate_at(vars(new_cases, total_cases), ~ . / population * 100000)
        }
        
        p1 <- plot_pred %>% 
            filter(name == "new_cases",
                   prob %in% input$quantile,
                   date <= input$date_range[2]) %>% 
            pivot_wider(names_from = which, values_from = value) %>% 
            ggplot(aes(date, ymin = lower, ymax = upper)) +
            geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
            # geom_line(data = plot_dat,
            #            aes(date, rolling_new_cases), inherit.aes = F, col = "grey", size = 2) +
            geom_point(data = plot_dat, inherit.aes = F, size = 0.8,
                       aes(date, new_cases,
                           colour = date > ymd("2020-10-03") + (location != "Iceland"),
                           text = str_c("Date: ", date, "<br>",
                                        "New cases: ", new_cases))) +
            scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
            scale_x_date(date_breaks = "month", labels = languages[[input$language]][["x_axis"]], expand = expansion(add = 7)) +
            scale_fill_brewer() +
            scale_colour_manual(values = c("black", "grey")) +
            theme(axis.title = element_blank(), plot.margin = margin(5, 5, 5, 10)) +
            coord_cartesian(xlim = c(pmax(min(plot_dat$date), input$date_range[1]), input$date_range[2]))
        
        p2 <- plot_pred %>% 
            filter(name == "total_cases",
                   prob %in% input$quantile,
                   date <= input$date_range[2]) %>% 
            pivot_wider(names_from = which, values_from = value) %>% 
            ggplot(aes(date, ymin = lower, ymax = upper)) +
            geom_ribbon(aes(fill = factor(-prob)), alpha = 0.8) +
            geom_point(data = plot_dat, inherit.aes = F, size = 0.8,
                       aes(date, total_cases,
                           colour = date > ymd("2020-10-03") + (location != "Iceland"),
                           text = str_c("Date: ", date, "<br>",
                                        "Total cases: ", total_cases))) +
            scale_y_continuous(breaks = pretty_breaks(8), expand = expansion(mult = 0.01)) +
            scale_x_date(date_breaks = "month", labels = languages[[input$language]][["x_axis"]], expand = expansion(add = 7)) +
            scale_fill_brewer() +
            scale_colour_manual(values = c("black", "grey")) +
            theme(axis.title = element_blank(), plot.margin = margin(5, 5, 5, 10)) +
            coord_cartesian(xlim = c(pmax(min(plot_dat$date), input$date_range[1]), input$date_range[2]))
        
        
        join_dat <- plot_dat %>% 
            group_by(location_id, wave_id) %>% 
            summarise(start_day = min(days),
                      end_day = max(days)) %>% 
            ungroup %>% 
            mutate(start_day = cumsum(lag(end_day + 1, n = 1, default = 0)),
                   end_day = ifelse(wave_id == max(wave_id), 9999, cumsum(end_day)) + 1)
        
        if (nrow(join_dat) > 1) {
            for (i in 2:nrow(join_dat)) {
                p1 <- p1 + geom_vline(xintercept = as.numeric(min(plot_dat$date) + join_dat$start_day[i]), linetype = 2)
                p2 <- p2 + geom_vline(xintercept = as.numeric(min(plot_dat$date) + join_dat$start_day[i]), lty = 2)
            }
        }
        
        return(list(p1, p2))
        
    })
    
    output$pred_plot <- renderPlotly({
        plots <- pred_plot()
        
        p1 <- plots[[1]]
        p2 <- plots[[2]]
        
        p1 <- ggplotly(p1, tooltip = "text")
        p2 <- ggplotly(p2, tooltip = "text")
        
        subplot(
            p1, 
            p2,
            nrows = 2
        )
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            str_c(input$location, ".", devices[[input$device]])
        },
        content = function(file) {  
            plots <- pred_plot()
            
            p1 <- plots[[1]] + 
                theme(plot.margin = margin(5, 5, 5, 16)) 
            p2 <- plots[[2]] + 
                theme(plot.margin = margin(5, 5, 5, 5))
            
            
            plot_grid(p1, p2, ncol = 1) +
                ggsave(file, device = devices[[input$device]], width = 5, height = 0.621 * 5, scale = 2.5, dpi = input$dpi)
        }
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
