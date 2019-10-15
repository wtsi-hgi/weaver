# Copyright (C) 2019  Genome Research Limited
# Author: Filip Makosza <fm12@sanger.ac.uk>
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(shiny)
library(tidyverse)
library(DT)
library(scales)

source("ggplot_formatter.R")

conf <- config::get("data")

connection <- DBI::dbConnect(RMariaDB::MariaDB(), 
  dbname = conf$database, 
  host = conf$host,
  port = conf$port,
  user = conf$username,
  password = conf$password)

on.exit(DBI::dbDisconnect(connection))

unique_dates <- tbl(connection, "lustre_usage") %>% select(`date`) %>% distinct() %>% collect() %>%
  # converts imported Date to a string
  transmute(date = as.character(date)) 

date_table_map <- list()
date_list <- list()

# creates a mapping of dates to report tables, allowing the user to change between dates easily
for(date_val in unique_dates$`date`){
  date_str <- as.character(date_val)
  date_list <- c(date_list, str_trim(date_str))
  
  date_table_map[[date_str]] <- tbl(connection, "lustre_usage") %>% filter(`date` == date_str) %>%
    select(c(`Lustre Volume`, `PI`, `Unix Group`, `Used (bytes)`, `Quota (bytes)`,
      `Consumption`, `Last Modified (days)`, `Archived Directories`, `IsHumgen`)) %>% 
    collect() %>%
    # converts columns imported as int64 to double, they play nicer with the rest of R
    mutate(`Quota (bytes)` = as.double(`Quota (bytes)`),
      `Used (bytes)` = as.double(`Used (bytes)`)) %>%
    # creates a secondary quota column which is easier to use internally than the
    # default Consumption column, never actually rendered to a table
    mutate(quota_use = na_if(`Used (bytes)`/`Quota (bytes)`, Inf),
      `Quota (bytes)` = na_if(`Quota (bytes)`, 0))
}

# sorts list of dates alphabetically, YYYY-MM-DD format means it's chronological
date_list <- str_sort(date_list, decreasing=TRUE)

# ONLY this form of indexing works here
volume_table <- date_table_map[[date_list[[1]]]]

# creates an empty table with the same column labels as volume_table
empty_tibble <- volume_table[0,]

# values to initialise UI elements to
maximum_size <- 1e15
maximum_age <- ceiling(max(volume_table$`Last Modified (days)`))

# negates %in% operator to use later
`%notin%` = Negate(`%in%`)

# creates list of dates to disable in date picker
date_index = lubridate::ymd( date_list[[length(date_list)]] )
blank_dates = c()
while(date_index != lubridate::ymd(date_list[[1]]) ) {
  if(toString(date_index) %notin% date_list){
    blank_dates = c(blank_dates, toString(date_index))
  }
  date_index = date_index + 1
}

# Helper to translate user inputs into numbers which can be passed into ggplot
parseBytes <- function(size, extension) {
  # Safeguard to stop log graph from crashing when a limit is negative or empty
  if(is.na(size) || size < 0) {
    return(0)
  }
  
  if(extension == "tb") {
    return(size*1024**4)
  } else if(extension == "gb") {
    return(size*1024**3)
  } else if(extension == "mb") {
    return(size*1024**2)
  } else if(extension == "kb") {
    return(size*1024)
  } else if(extension == "b") {
    return(size)
  }
}

# custom transformation used to simultaneously reverse and logify a graph axis
reverse_log10_trans <- scales::trans_new(
  name = "reverse_log10",
  transform = function(x){ return(-log10(x)) },
  inverse = function(x){ return(10^(-x)) }
);

# -------------------- UI -------------------- #
ui <- fluidPage(
  br(),
  fluidRow(
    # Left hand side, top panel
    column(4,
      tabsetPanel(
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
                choices = list("TiB" = "tb",
                  "GiB" = "gb",
                  "MiB" = "mb",
                  "KiB" = "kb",
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
                choices = list("TiB" = "tb",
                  "GiB" = "gb",
                  "MiB" = "mb",
                  "KiB" = "kb",
                  "B" = "b"),
                selected="tb"
              )
            )
          ),
          
          sliderInput("filter_lastmodified",
            "Last Modified (days)",
            min=0, max=maximum_age, value=c(0, maximum_age)
          ),
          selectInput("filter_archived", "Show archived directories?",
            choices = list("Yes", "No", "Only"), selected = "Yes"
          ),
          selectInput("filter_humgen", "Show non-Humgen groups?",
            choices = list("Yes", "No", "Only"), selected = "No"
          ),
          actionButton("clear_filters", "Clear filters"),
          br(), br()
        ),
        tabPanel("Modifiers",
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
            choices = list("Scatter" = "scatter", "Cumulative Histogram" = "histogram"),
            selected = "scatter"
          ),
          
          conditionalPanel("input.graph_selector == 'histogram'",
            numericInput("histogram_bins", h4("Histogram bin count"), value=40)
          )
        ),
        tabPanel("Help",
          h6("Click or click and drag on the graph to select data points. Your selection will
            appear in a table at the bottom of the page. Click on a blank space to clear the
			selection."),
          h6("Click on a row within a table to highlight the corresponding
            data point in red on the graph."),
          h6("The tables are locked to a particular height to stop the page from
            moving around when interacting with the graph. Make sure not to miss
            the scroll bar on tall selection tables.")
        )
      ) #Tabset panel end
    ), # Left hand side top panel end
    
    column(8,
      dateInput("date_picker", NULL, value = date_list[[1]],
        min = date_list[[length(date_list)]],
        max = date_list[[1]],
        datesdisabled = blank_dates
      ),
      plotOutput("ui_volume_graph",
        click = "graph_click",
        brush = brushOpts(id = "graph_brush", resetOnNew=FALSE)
      ),
      textOutput("ui_selection_size")
    )
  ),
  hr(style="border-color:black;"),
  fluidRow(
    actionButton("clear_full", "Clear selection"),
    br(), br(),
    DTOutput("ui_volume_table"),
    downloadButton("downloadFull", "Download full report"),
    downloadButton("downloadTable", "Download table"),
    br(), br()
  )
)

# -------------------- SERVER -------------------- #
server <- function(input, output, session) {
  # Construct the graph step by step based on user input
  assemblePlot <- function() {
    
    if(input$graph_selector == "scatter") {
      
      # Displays no-value error regardless of which column is used, Used (bytes)
      # column is used arbitrarily
      validate(need(filtered_table()$`Used (bytes)`, "No values to plot!"))
      
      volume_plotter <- ggplot() + 
        geom_point(filtered_table(),
          mapping = aes(x= `Last Modified (days)`,
            y= `Used (bytes)`,
            alpha = 0.1)) + scale_alpha(guide="none") +
        scale_y_continuous(labels=filesize_format)
        
      # Renders points corresponding to clicked table rows in red
      
      table_selection <- getSelection()[input$ui_volume_table_rows_selected, ]
      
      volume_plotter <- volume_plotter + geom_point(table_selection,
        mapping = aes(x= `Last Modified (days)`,
          y= `Used (bytes)`), size = 2, colour = "red")
      
    } else if (input$graph_selector == "histogram") {
      
      volume_plotter <- ggplot(filtered_table(), aes(`Last Modified (days)`)) +
        # Histogram bar height is weighted by file size
        geom_histogram(aes(y=cumsum(..count..), weight=`Used (bytes)`), bins= input$histogram_bins) +
        ylab("Used (bytes)") +
        scale_x_reverse() +
        scale_y_continuous(labels=filesize_format)
    }
    
    if(input$log_x) {
      volume_plotter <- volume_plotter + scale_x_continuous(trans=reverse_log10_trans,
        # not elegant, but this should work for about 30 years anyway
        breaks = c(1, 5, 10, 50, 100, 500, 1000, 5000, 10000))
    }
    
    # Makes sure histogram y axis isn't logified if the user logifies the scatter plot
    # y axis and switches to histogram view
    if(input$log_y && input$graph_selector != "histogram"){
      # stops warning spam of scales being overwritten
      suppressMessages(
        # this overrides scale_x_reverse from a few lines back
        volume_plotter <- volume_plotter + scale_y_continuous(trans="log10", labels=filesize_format)
      )
    }
    
    return(volume_plotter)
  }
  
  # Filters a table based on parameters given by the user, used to reduce
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
    
    if(input$filter_archived == "No"){
      filtered_graph_table <- filter(filtered_graph_table, is.na(`Archived Directories`)) 
    } else if(input$filter_archived == "Only") {
      filtered_graph_table <- filter(filtered_graph_table, !is.na(`Archived Directories`))
    }
    
    if(input$filter_humgen == "No") {
      filtered_graph_table <- filter(filtered_graph_table, `IsHumgen` == 1)
    } else if(input$filter_humgen == "Only") {
      filtered_graph_table <- filter(filtered_graph_table, `IsHumgen` == 0)
    }
    
    return(filtered_graph_table)
  }
 
  
  observeEvent(input$clear_full, {
    dataTableProxy("ui_volume_table") %>% selectRows(NULL)
  })
  
  observeEvent(input$clear_filters,{
    updateTextInput(session, "filter_lustrevolume", value = "")
    updateTextInput(session, "filter_pi", value="")
    updateTextInput(session, "filter_unixgroup", value="")
    updateNumericInput(session, "filter_size_to", value=1000)
    updateSelectInput(session, "filter_size_to_unit", selected="tb")
    updateNumericInput(session, "filter_size_from", value=0)
    updateSelectInput(session, "filter_size_from_unit", selected="tb")
    updateSliderInput(session, "filter_lastmodified", value=c(0, maximum_age))
    updateSelectInput(session, "filter_archived", selected="Yes")
    updateSelectInput(session, "filter_humgen", selected="No")
  })
  
  volume_table <- eventReactive(input$date_picker,{
    date_table_map[[toString(input$date_picker)]]
  })
  
  observeEvent(input$date_picker, {
    maximum_age <- ceiling(max(volume_table()$`Last Modified (days)`))
    # only updates the maximum slider value if it's smaller than this date's oldest volume
    if(maximum_age > input$filter_lastmodified[2]){
      updateSliderInput(session, "filter_lastmodified", max=maximum_age, value=c(0,maximum_age))
    }
  })
  
  filtered_table <- eventReactive(
    c(input$filter_lustrevolume,
      input$filter_pi,
      input$filter_unixgroup,
      input$filter_size_to,
      input$filter_size_to_unit,
      input$filter_size_from,
      input$filter_size_from_unit,
      input$filter_lastmodified,
      input$filter_archived,
      input$date_picker,
      input$filter_humgen), {
        filterTable(volume_table())
      }, ignoreNULL = FALSE
  )
  
  
  output$ui_volume_graph <- renderPlot(assemblePlot())
  
  output$ui_volume_table <- renderDT(
    datatable(getSelection(),
      options = list(pageLength=10,
        # Makes the sixth (1-indexed) column (Consumption) sort by the values of hidden
        # ninth column (quota_use) calculated at the top of the app
        columnDefs = list(
          list(orderData=10, targets=6),
          list(targets=c(9,10), visible=F, searchable=F),
          list(targets=c(4, 5, 6), searchable=F)
        ),
        scrollY = "650px",
        searching = FALSE
      )
    # hack to make the byte columns render with comma separators
    ) %>% formatCurrency(4:5, currency="", digits=0)
  )

  # -------------------------
  # This code chunk is used to figure out what data points the user last
  # selected, and then renders them to a graph
  
  reactive_select <- reactiveValues(event_flag = "", selection = empty_tibble)
  # priority option is used to ensure that event_flag modifying observers execute
  # before the selection picking observer
  observeEvent(input$graph_click, priority = 10, {
    reactive_select[['event_flag']] <- "click"
  })

  observeEvent(input$graph_brush, priority = 10, {
    reactive_select[['event_flag']] <- "brush"
  })
  
  observeEvent(input$date_picker, priority = 10, {
    similar_elements <- semi_join(filtered_table(), reactive_select[['selection']],
      by=c('Lustre Volume', 'PI', 'Unix Group'))
    reactive_select[['selection']] <- similar_elements
  })
  
  observeEvent(c(input$graph_brush, input$graph_click), priority = 9, {
    
    if(reactive_select[['event_flag']] == ""){
      reactive_select[['selection']] <- empty_tibble
    
    } else if(reactive_select[['event_flag']] == "click") {
      reactive_select[['selection']] <- nearPoints(filtered_table(), input$graph_click)
    
    } else if(reactive_select[['event_flag']] == "brush") {
      reactive_select[['selection']] <- brushedPoints(filtered_table(), input$graph_brush)
    }
  })
  
  # this is the only function anything outside this code chunk should have to use
  getSelection <- reactive({
    if(input$graph_selector == "scatter") {
      if(nrow(reactive_select[['selection']]) == 0){
        return(filtered_table())
      } else {
        return(reactive_select[['selection']])
      }
    } else {
      return(filtered_table())
    }
  })
  
  # -------------------------
  
  output$downloadFull <- downloadHandler(
    filename = function() {
      # format the filename as 'report-YYYYMMDD.tsv'
      paste("report-", str_replace_all(input$date_picker, '-', ''), ".tsv", sep="")
    },
    content = function(file) {
      # exclude hidden quota_use column from file
      write.table(select(volume_table(), -quota_use),
        file, quote=FALSE, sep="\t", na="-", row.names=FALSE)
    }
  )
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("report-", str_replace_all(input$date_picker, '-', ''), ".tsv", sep="")
    },
    content = function(file) {
      write.table(select(getSelection(), -quota_use),
        file, quote=FALSE, sep="\t", na="-", row.names=FALSE)
    }
  )
  
  # Highlights all the rows in a table (making the graphed points red) only if
  # the user just clicked
  # FIXME: doesn't work since table unification
  getSelectionIfClicked <- function() {
    if(reactive_select[['event_flag']] != "brush"){
      return(list(mode='multiple', selected = c(0:nrow(getSelection()))))
    } else {
      return(list(mode='multiple'))
    }
  }
  
  # Returns the total size of volumes in a selection in terabytes
  getSelectionSize <- function() {
    selection <- getSelection()
    sizeofSelection <- sum(selection$`Used (bytes)`) / 1024**4
    return(sizeofSelection)
  }
  
  # Returns the total amount of volumes in a selection
  getSelectionCount <- function() {
    selection <- getSelection()
    countofSelection <- nrow(selection)
    return(countofSelection)
  }
  
  output$ui_selection_size <- renderText(sprintf("Selection: %.2f TiB stored in %s volumes", 
    getSelectionSize(),
    getSelectionCount()))
}

shinyApp(ui=ui, server=server)
