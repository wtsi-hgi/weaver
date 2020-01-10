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
library(rhandsontable)

DataGenerator <- setRefClass( "dataClass",
  fields = c('data', 'conf'),
  methods = list(
    updateData = function() {
      # can be used to keep the data fresh without restarting the program
      # NOTE: the connection has to be reestablished in the object each time,
      # storing and passing it around between functions crashes the program
      connection <- DBI::dbConnect(RMariaDB::MariaDB(),
        dbname = conf$database,
        host = conf$host,
        port = conf$port,
        user = conf$username,
        password = conf$password
      )
      
      data <<- tbl(connection, "spaceman") %>% collect()
      
      DBI::dbDisconnect(connection)
      
    },
    writeData = function(table) {
      # finds the rows that were changed and writes only them
      changed_rows <- anti_join(table, data)
      
      connection <- DBI::dbConnect(RMariaDB::MariaDB(),
        dbname = conf$database,
        host = conf$host,
        port = conf$port,
        user = conf$username,
        password = conf$password
      )
      
      for (row in transpose(changed_rows)) {
        req <- RMariaDB::dbSendStatement(connection, 
          "UPDATE spaceman SET `Action` = ?, `Comment` = ? WHERE `index` = ?", 
          param = list(row$Action, row$Comment, row$index)
        )
        
        RMariaDB::dbClearResult(req)
      }
      
      DBI::dbDisconnect(connection)
      updateData()
    },
    getScratches = function() {
      scratches <- select(data, c(`Volume`)) %>% distinct()
      scratches <- as.list(scratches)
      return(scratches)
    },
    getProjects = function(volume) {
      projects <- filter(data, `Volume` == volume & `Project` != "projects" & `Project` != "teams") %>% select(c(`Project`)) %>% distinct()
      projects <- as.list(projects)
      return(projects)
    },
    getPIs = function() {
      PIs <- select(data, c(`PI`)) %>% distinct()
      PIs <- as.list(PIs)
      return(PIs)
    },
    getGroups = function(volume) {
      if (volume == ""){
        groups <- select(data, c(`Unix Group`)) %>% distinct()
      } else {
        groups <- filter(data, `Volume` == volume) %>% select(c(`Unix Group`)) %>% distinct()
      }
        
      groups <- as.list(groups)
      return(groups)
    },
    getDirectories = function(volume, project, pi, group) {
      if ((volume == "" | project == "") & (pi == "") & (group == "")){
        directories <- filter(data, FALSE) %>%
          select(c(`index`, `Volume`, `Project`, `Directory`, `Files`, `Total`, `BAM`, `CRAM`, `VCF`, `PEDBED`, `Last Modified (days)`, `PI`, `Unix Group`, `Status`, `Action`, `Comment`))
        
      } else {
        directories <- filter(data, `Directory` != "*TOTAL*") %>%
          filter(if (volume != "") `Volume` == volume else TRUE) %>%
          filter(if (project != "") `Project` == project else TRUE) %>%
          filter(if (pi != "") `PI` == pi else TRUE) %>%
          filter(if (group != "") `Unix Group` == group else TRUE) %>%
          select(c(`index`, `Volume`, `Project`, `Directory`, `Files`, `Total`, `BAM`, `CRAM`, `VCF`, `PEDBED`, `Last Modified (days)`, `PI`, `Unix Group`, `Status`, `Action`, `Comment`)) 
      }
      
      return(directories)
    },
    getProjectStats = function(volume, project) {
      entry <- filter(data, `Volume` == volume & `Project` == project & `Directory` == "*TOTAL*")
      return(entry)
    }
  )
)

data <- DataGenerator$new(conf=config::get("data"))
data$updateData()

ui <- fluidPage(
  # stops read-only cells (aka most of them) rendering as grey and faded
  tags$head(
    tags$style(
      HTML(
        ".handsontable td.htDimmed {
          color: black
        }"
      )
    )
  ),
  
  br(),
  fluidRow(
    column(5,
      selectInput("volume", "Lustre Volume",
        choices = c("-" = "", data$getScratches())
      )
    )
  ),
  fluidRow(
    column(12,
      selectInput("project", "Project Directory",
        choices = c("-" = "")  
      )
    )
  ),
  fluidRow(
    column(5,
      selectInput("pi", "PI",
        choices = c("-" = "", data$getPIs())
      )
    ),
    column(5,
      selectInput("group", "Unix Group",
        choices = c("-" = "", data$getGroups(""))
      )
    )
  ),
  fluidRow(
    column(5,
      uiOutput("link")
    )
  ),
  fluidRow(
    column(12,
      h4(textOutput("size_summary")),
      h4(textOutput("file_summary")),
      h4(textOutput("pi_summary")),
      br(),
      h4("Subdirectories:"), 
      rHandsontableOutput("table"),
      br(), br()
    )
  )
)

server <- function(input, output, session) {
  # try to update the data at the start of each session
  data$updateData()
  
  observeEvent(session$clientData$url_search, {
    val_pairs <- str_split(session$clientData$url_search, fixed("?"), simplify=TRUE)

    volume <- ""
    group <- ""
    PI <- ""
    
    for (pair in val_pairs) {
      if (pair == ""){
        
      } else {
        pair <- str_split(pair, fixed("="), simplify=TRUE)
        if (pair[[1]] == "volume") {
          volume <- pair[[2]]
        } else if (pair[[1]] == "group") {
          group <- pair[[2]]
        } else if (pair[[1]] == "pi") {
          PI <- pair[[2]]
        }
      }
    }
    updateSelectInput(session, "volume", selected = volume)
    updateSelectInput(session, "group", choices = c("-" = "", data$getGroups(volume)),
      selected = group)
    updateSelectInput(session, "pi", selected = PI)
  })
  
  output$link <- renderUI({
    volume <- ""
    if (input$volume != ""){
      volume <- sprintf("?volume=scratch%s", input$volume)
    }
    
    group <- ""
    if (input$group != ""){
      group <- sprintf("?group=%s", input$group)
    }
    
    pi <- ""
    if (input$pi != ""){
      pi <- sprintf("?pi=%s", input$pi)
    }
    
    click <- sprintf("location.href='/weaver%s%s%s'", volume, group, pi)
    
    actionButton("link", "Group Lustre usage report",
      onclick=click)
  })
  
  observeEvent(input$volume, {
    if (input$project %in% data$getProjects(input$volume)$Project) {
      updateSelectInput(session, "project", choices = c("-" = "", data$getProjects(input$volume)),
        selected = input$project)
    } else {
      updateSelectInput(session, "project", choices = c("-" = "", data$getProjects(input$volume)))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$project, {
    if (input$project != ""){
      summary <- data$getProjectStats(input$volume, input$project) %>% select(c(`Total`, `Files`, `PI`, `index`)) %>% as.list()
      size <- round(as.numeric(summary$Total), 3)
      files <- format(as.numeric(summary$Files), big.mark = ",", scientific=FALSE)
      
      output$size_summary <- renderText({str_interp("Total size: ${size} TiB")})
      output$file_summary <- renderText({str_interp("Total files: ${files}")})
      output$pi_summary <- renderText({str_interp("PI: ${summary$PI}")})
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$table, {
    # hot_to_r crashes if the table is empty, this tryCatch just causes those
    # crashes to be ignored
    tryCatch(
      data$writeData(hot_to_r(input$table)),
      error=function(cond){return(NULL)}
      )
  })
  
  getDirs <- reactive({
    invalidateLater(1000, session)
    data$updateData()
    data$getDirectories(input$volume, input$project, input$pi, input$group)
  })
  
  output$table <- renderRHandsontable(
    rhandsontable(
      getDirs()
    ) %>% 
      hot_cols(readOnly = TRUE) %>%
      hot_col(col = "Action", type = "dropdown", readOnly = FALSE , source = list("no decision", "keep", "archive", "delete")) %>%
      hot_col(col = "Comment", readOnly = FALSE) %>%
      # hack to make the index column invisible (column hiding is a Handsontable Pro
      # feature and not available in rhandsontable)
      hot_col(col = "index", colWidths=0.1)
  ) 
}

shinyApp(ui=ui, server=server)
