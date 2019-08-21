library(shiny)
library(tidyverse)

volume_table <- read_tsv("report-20190819.tsv", na="-")
#Filter out entries where log10(volume size) produces an unusable number
graph_table <- filter(volume_table, `Used (bytes)` >= 1)

ui <- fluidPage(
  fluidRow(
    column(4, h4("Axes to scale logarithmically"),
           fluidRow(
             column(6,
                    checkboxInput("log_x", "Last Modified", value=FALSE)
             ),
             
             column(6,
                    # Don't show y-axis logifier in histogram mode, it freaks out at values <1
                    conditionalPanel("input.graph_selector == 'scatter'",
                                     checkboxInput("log_y", "Volume Size", value=FALSE)
                    )
             ) 
           ),
           radioButtons("graph_selector", h4("Graph type"),
                        choices = list("Scatter" = "scatter", "Histogram" = "histogram"),
                        selected = "scatter"
                        ),
           
           conditionalPanel("input.graph_selector == 'histogram'",
                            numericInput("histogram_bins", h4("Histogram bin count"), value = 40)
                            )
           ),
    
    column(8,
           plotOutput("ui_volume_graph")
           )
  ),
  
  fluidRow(
    column(12,
           dataTableOutput("ui_volume_table")
           )
  )
  
)

server <- function(input, output){
  # Construct the graph step by step based on user input
  assemblePlot <- function(){
    if(input$graph_selector == "scatter"){
      volume_plotter <- ggplot(graph_table,
                               aes(x = `Last Modified (days)`,
                                   y= `Used (bytes)`,
                                   alpha = 0.1)) + geom_point()
    } else if (input$graph_selector == "histogram"){
      volume_plotter <- ggplot(graph_table, aes(`Last Modified (days)`)) +
        # Histogram bar height is weighted by file size
        geom_histogram(aes(weight=`Used (bytes)`), bins= input$histogram_bins) +
        # Explicitly set y axis label, would be "count" otherwise
        ylab("Used (bytes)") 
    }
    
    if(input$log_x){
      volume_plotter <- volume_plotter + scale_x_continuous(trans="log10")
    }
    # Makes sure histogram y axis isn't logified if the user logifies the scatter plot
    # y axis and switches to histogram view
    if(input$log_y && input$graph_selector != "histogram"){
      volume_plotter <- volume_plotter + scale_y_continuous(trans="log10")
    }
    
    volume_plotter # Makes the function return volume_plotter
  }
  
  output$ui_volume_graph <- renderPlot(assemblePlot())
  
  output$ui_volume_table <- renderDataTable(volume_table)
}

shinyApp(ui=ui, server=server)