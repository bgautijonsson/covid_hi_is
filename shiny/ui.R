library(shiny)
library(gridExtra)
library(shinythemes)

shinyUI(fluidPage( 
    theme = shinytheme("flatly"),
    
    navbarPage("COVID-19 Sviðsmyndir",
        tabPanel("Sviðsmyndir",
            sidebarLayout(
                sidebarPanel(
                    h4('Aðgerð yfirvalda'),
                    checkboxInput("quarantine", "Virk smitrakning", F),
                    
                    radioButtons("test", "Skimun við landamærin:",
                                 c("Engin skimun" = "none",
                                    "Ein skimun" = "first",
                                   "Tvær skimarnir fyrir Íslendinga" = "second_ice",
                                   "Tvær skimanir fyrir alla" = "second_all")),
                    actionButton('go',label="Teikna myndir"),
                    br(),
                    br(),
                    br(),
                    downloadButton('downloadPlot', label = "Hlaða niður mynd sem PDF"),
                    br(),
                    br(),
                    downloadButton('downloadCSV', label='Hlaða niður gögnum sem XLSX')
                ),
                
                mainPanel(
                    plotOutput('epidemia_plot',height = '800px')
                )
            )
        ),
        tabPanel('Um verkefnið',
            mainPanel(
                h4('Um verkefnið')
            )         
        )
    )
))
