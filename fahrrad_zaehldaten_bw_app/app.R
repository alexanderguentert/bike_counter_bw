#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

source <- 'https://mobidata-bw.de/daten/eco-counter/eco_counter_fahrradzaehler.csv'
f <- read.csv(source, header=TRUE, sep=',')
f$time <- strptime(f$timestamp,format = '%Y-%m-%dT%H:%M:%S')

ui <- fluidPage(
    headerPanel('Example'),
    sidebarPanel(
        selectInput('xcol','X Variable', names(mtcars)),
        selectInput('ycol','Y Variable', names(mtcars)),
        selectInput('standort','Standort', unique(f$standort)),
        selected = names(mtcars)[[2]]),
    mainPanel(
        plotlyOutput('plot'),
    )
)

server <- function(input, output) {
    
    x <- reactive({
        mtcars[,input$xcol]
    })
    
    y <- reactive({
        mtcars[,input$ycol]
    })
    
    
    output$plot <- renderPlotly(
        plot1 <- plot_ly(
            x = x(),
            y = y(), 
            type = 'scatter',
            mode = 'markers')
    )
    
}
# Run the application 
shinyApp(ui = ui, server = server)
