library(shiny)
library(tidyverse)
library(DT)

# volume_table is always used in the full data table
volume_table <- read_tsv("report-20190819.tsv", na="-")

# Creates secondary quota column which is easier to use internally than 
# default Consumption column
volume_table <- mutate(volume_table,
  quota_use = na_if(`Used (bytes)`/`Quota (bytes)`, Inf),
  `Quota (bytes)` = na_if(`Quota (bytes)`, 0))

# graph_table is altered and always used to create graphs
# Filter out entries where log10(volume size) produces an unusable number
# NOTE: Empty volumes DO NOT appear on the graph!
graph_table <- filter(volume_table, `Used (bytes)` >= 1)

maximum_size <- max(graph_table$`Used (bytes)`)
maximum_age <- ceiling(max(graph_table$`Last Modified (days)`))

# Helper to translate user inputs into numbers which can be passed into ggplot
parseBytes <- function(size, extension) {
  # Safeguard to stop log graph from crashing when a limit is 0 or empty
  if(is.na(size) || size == 0) {
    return(1)
  }
  
  if(extension == "tb") {
    return(size*1e12)
  } else if(extension == "gb") {
    return(size*1e9)
  } else if(extension == "mb") {
    return(size*1e6)
  } else if(extension == "kb") {
    return(size*1e3)
  } else if(extension == "b") {
    return(size)
  }
}

# -------------------- UI -------------------- #
ui <- fluidPage(
  fluidRow(
    # Left hand side, top panel
    column(4,
      tabsetPanel(
        tabPanel("Graph",
          h4("Axes to scale logarithmically"),
          
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
            numericInput("histogram_bins", h4("Histogram bin count"), value=40)
            ),
          
          conditionalPanel("input.graph_selector == 'scatter'",
            h4("Volume size range"),
            
            # TODO: Abstract this into filesize selector
            fluidRow(
              column(8, 
                numericInput("size_from", label=NULL, value=0)
                ),
              
              column(4,
                selectInput("size_from_unit", label=NULL,
                  choices = list("TB" = "tb",
                    "GB" = "gb",
                    "MB" = "mb",
                    "KB" = "kb",
                    "B" = "b"),
                  selected="tb"
                  )
                )
              ),
                                 
            fluidRow(
              column(8, 
                numericInput("size_to", label=NULL,
                  value=ceiling(maximum_size/1e12)
                  )
                ),
              
              column(4, 
                selectInput("size_to_unit", label=NULL,
                  choices = list("TB" = "tb",
                    "GB" = "gb",
                    "MB" = "mb",
                    "KB" = "kb",
                    "B" = "b"),
                  selected="tb"
                  )
                )
              )
            )
          ),
        
        tabPanel("Data",
          h4("Data filters"),
          textInput("filter_lustrevolume",
            "Lustre Volume"
            ),
          textInput("filter_pi",
            "PI"
            ),
          textInput("filter_unixgroup",
            "Unix Group"
            ),
          
          # Volume size selector - basically a copy-paste from the code used to change graph
          # axis range
          tags$strong("Volume size range"),
          fluidRow(
            column(8, 
              numericInput("filter_size_to", label=NULL,
                value=ceiling(maximum_size/1e12)
              )
            ),
            
            column(4, 
              selectInput("filter_size_to_unit", label=NULL,
                choices = list("TB" = "tb",
                  "GB" = "gb",
                  "MB" = "mb",
                  "KB" = "kb",
                  "B" = "b"),
                selected="tb"
              )
            )
          ),
          
          fluidRow(
            column(8, 
              numericInput("filter_size_from", label=NULL,
                value=0
              )
            ),
            
            column(4, 
              selectInput("filter_size_from_unit", label=NULL,
                choices = list("TB" = "tb",
                  "GB" = "gb",
                  "MB" = "mb",
                  "KB" = "kb",
                  "B" = "b"),
                selected="tb"
              )
            )
          ),
          
          sliderInput("filter_lastmodified",
            "Last Modified (days)",
            min=0, max=maximum_age, value=c(0, maximum_age)
            ),
          checkboxInput("filter_archived",
            "Show archived volumes?",
            value=TRUE
            )
          )
        ) #Tabset panel end
      ), # Left hand side top panel end
    
    column(8,
      plotOutput("ui_volume_graph",
        click = "graph_click",
        brush = brushOpts(id = "graph_brush", resetOnNew=TRUE)
        ),
      textOutput("ui_selection_size")
      )
    ),
  
  fluidRow(
    tabsetPanel( selected = "Selection",
      tabPanel("Full Table", DTOutput("ui_volume_table")),
      tabPanel("Selection", DTOutput("ui_selection_table"))
      )
    )
)

# -------------------- SERVER -------------------- #
server <- function(input, output) {
  # Construct the graph step by step based on user input
  assemblePlot <- function() {
    
    if(input$graph_selector == "scatter") {
      
      from <- parseBytes(input$size_from, input$size_from_unit)
      to <- parseBytes(input$size_to, input$size_to_unit)
      
      # Displays error regardless of which filters are used, volume size column
      # is used arbitrarily
      validate(need(filtered_table()$`Used (bytes)`, "No values to plot!"))
      
      volume_plotter <- ggplot(filtered_table(),
                               aes(x= `Last Modified (days)`,
                                   y= `Used (bytes)`,
                                   alpha = 0.1)) + 
        geom_point() + scale_alpha(guide="none") +
        # TODO: Replace with scale_y_continuous(limits=)? Would hide out of bound
        # data points, not just resize graph (useful for log graphs)
        coord_cartesian(ylim=c(from, to)) +
        # Renders lines at limits
        geom_hline(yintercept=from, linetype="dashed") +
        geom_hline(yintercept=to, linetype="dashed")
      
    } else if (input$graph_selector == "histogram") {
      
      volume_plotter <- ggplot(filtered_table(), aes(`Last Modified (days)`)) +
        # Histogram bar height is weighted by file size
        geom_histogram(aes(y=cumsum(..count..), weight=`Used (bytes)`), bins= input$histogram_bins) +
        # Explicitly set y axis label, would be "count" otherwise
        ylab("Used (bytes)") +
        scale_x_reverse()
    }
    
    if(input$log_x) {
      volume_plotter <- volume_plotter + scale_x_continuous(trans="log10")
    }
    # Makes sure histogram y axis isn't logified if the user logifies the scatter plot
    # y axis and switches to histogram view
    if(input$log_y && input$graph_selector != "histogram"){
      volume_plotter <- volume_plotter + scale_y_continuous(trans="log10")
    }
    
    volume_plotter # Makes the function return volume_plotter
  }
  
  # Filters a table based on parameters given by the user, to be used to reduce
  # the number of data points on a scatter graph
  filterTable <- function(table_in){
    
    filtered_graph_table <- table_in
    
    if(nchar(input$filter_lustrevolume) > 0){
      filtered_graph_table <- filter(filtered_graph_table, 
        str_detect(`Lustre Volume`, coll(input$filter_lustrevolume, ignore_case = T)))
    }

    if(nchar(input$filter_pi) > 0){
      filtered_graph_table <- filter(filtered_graph_table,
        str_detect(`PI`, coll(input$filter_pi, ignore_case = T)))
    }
    
    if(nchar(input$filter_unixgroup) > 0){
      filtered_graph_table <- filter(filtered_graph_table,
        str_detect(`Unix Group`, coll(input$filter_unixgroup, ignore_case = T)))
    }
    
    from <- parseBytes(input$filter_size_from, input$filter_size_from_unit)
    to <- parseBytes(input$filter_size_to, input$filter_size_to_unit)
    if(from < to){
      filtered_graph_table <- filter(filtered_graph_table, between(`Used (bytes)`, from, to))
    } else {
      filtered_graph_table <- filter(filtered_graph_table, between(`Used (bytes)`, to, from))
    }
    
    filtered_graph_table <- filter(filtered_graph_table,
      between(`Last Modified (days)`, input$filter_lastmodified[1], input$filter_lastmodified[2]))
    
    if(input$filter_archived){
      filtered_graph_table <- filter(filtered_graph_table, is.na(`Archived Directories`)) 
    }
    
    return(filtered_graph_table)
  }
  
  # Disables input table creation when not in scatter graph mode
  getSelection <- function() {
    if(input$graph_selector == "scatter") {
      input$graph_brush
    }
  }
  
  # Returns the total size of volumes in a selection in terabytes
  getSelectionSize <- function() {
    selection <- brushedPoints(filtered_table(), getSelection())
    sizeofSelection <- sum(selection$`Used (bytes)`) / 1e12
    sizeofSelection
  }
  
  # Returns the total amount of volumes in a selection
  getSelectionCount <- function() {
    selection <- brushedPoints(filtered_table(), getSelection())
    countofSelection <- nrow(selection)
    countofSelection
  }
  
  filtered_table <- graph_table
  
  filtered_table <- eventReactive(
    c(input$filter_lustrevolume,
      input$filter_pi,
      input$filter_unixgroup,
      input$filter_size_to,
      input$filter_size_to_unit,
      input$filter_size_from,
      input$filter_size_from_unit,
      input$filter_lastmodified,
      input$filter_archived), {
        filterTable(graph_table)
      }, ignoreNULL = FALSE
    )
  
  output$ui_volume_graph <- renderPlot(assemblePlot())
  
  output$ui_volume_table <- renderDT(volume_table, 
    options = list(pageLength=10,
      # Makes the sixth (1-indexed) column (human-readable Consumption) sort by the values of hidden
      # ninth column quota_use calculated at the top of the app
      columnDefs = list(
        list(orderData=9, targets=6),
        list(targets=9, visible=F, searchable=F),
        list(targets=6, searchable=F)
      )
    ),
    filter = list(position="bottom", clear=FALSE)
  )
  
  output$ui_selection_table <- renderDT(
    brushedPoints(filtered_table(), getSelection()),
    options = list(pageLength=10,
      # Same as above, but for the selection table
      columnDefs = list(
        list(orderData=9, targets=6),
        list(targets=9, visible=F, searchable=F),
        list(targets=6, searchable=F)
      )
    ),
    filter = list(position="bottom", clear=FALSE)
  )
  
  output$ui_selection_size <- renderText(sprintf("Selection: %.2f TB stored in %s volumes", 
                                                 getSelectionSize(),
                                                 getSelectionCount()))
}

shinyApp(ui=ui, server=server)
