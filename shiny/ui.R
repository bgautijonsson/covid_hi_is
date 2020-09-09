library(shiny)
library(gridExtra)

shinyUI(fluidPage( 
    includeCSS("styles.css"),
    
    titlePanel("Epidemia Plot Generator"),
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
            downloadButton('downloadPlot', label = "Download plot as PDF"),
            br(),
            br(),
            downloadButton('downloadCSV', label='Download estimate as CSV')
        ),
        
        mainPanel(
            plotOutput('epidemia_plot')
        )
    )
))
