library(shiny)
library(gridExtra)

shinyUI(fluidPage( 
    includeCSS("styles.css"),
    
    titlePanel("COVID-19 Sviðsmyndir"),
    sidebarLayout(
        sidebarPanel(
            h4('Aðgerð yfirvalda'),
            checkboxInput("quarantine", "Virk smitrakning", F),
            
            radioButtons("test", "Skimun við landamærin:",
                         c("Engin skimun" = "none",
                            "Eingöngu fyrsta skimun" = "first",
                           "Fyrsta skimun og seinni skimun fyrir Íslendinga" = "second_ice",
                           "Bæði seinni og fyrri skimun fyrir alla sem koma til landsins" = "second_all")),
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
            plotOutput('epidemia_plot')
        )
    )
))
