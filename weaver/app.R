# Copyright (C) 2019, 2021  Genome Research Limited
# Author: 
#   - Filip Makosza <fm12@sanger.ac.uk>
#   - Michael Grace <mg38@sanger.ac.uk>
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
library(shinyjs)
library(tidyverse)
library(DT)
library(scales)

source("ggplot_formatter.R")
source("helpers.R")
source("predictions.R")
source("ui.R")
source("db.R")
source("directories.R")
source("vault.R")
source("users.R")

# negates %in% operator to use later
`%notin%` = Negate(`%in%`)

# --- DATABASE AND GETTING INFO ---

conf <<- config::get("data")

regenDBData <- function() {
  connection <<- DBI::dbConnect(RMariaDB::MariaDB(),
    dbname = conf$database,
    host = conf$host,
    port = conf$port,
    user = conf$username,
    password = conf$password
  )

  # TODO: we want to close the connection, but it breaks everything cause shiny
  # on.exit(DBI::dbDisconnect(connection))

  # Create a load of tables containing foreign keys
  # and their respective values (i.e. names)
  pis <<- tbl(connection, "pi") %>%
    select(c('pi_id', 'pi_name'))

  unix_groups <<- tbl(connection, "unix_group") %>%
    select(c('group_id', 'group_name', "is_humgen"))

  volumes <<- tbl(connection, "volume") %>%
    select(c('volume_id', 'scratch_disk'))

  vault_actions <<- tbl(connection, "vault_actions")  %>% 
    select(c("vault_action_id", "action_name"))

  # Loads the main datatable
  volume_table <<- loadDBData(connection)
  empty_tibble <<- volume_table[0,]

  # values to initialise UI elements to
  maximum_size <<- 1e15
  # rounds maximum age up to nearest thousand
  maximum_age <<- ceiling(max(volume_table$`last_modified`)/1000)*1000 

}

# -------------------- SERVER -------------------- #
server <- function(input, output, session) {
  withProgress(
    message = "Loading...",
    min = 0,
    max = 0,
    {
      regenDBData()
    }
  )

  # Sets some default values
  output$detailed_report_title <- renderText({"Please select a record to view more..."})
  output$warnings_summary_name <- renderText({"Please select a PI or Lustre Volume on the left"})
  output$result_dates <- renderTable(loadScratchDates(connection), colnames = FALSE)
  shinyjs::hide("pred_date")
  shinyjs::hide("detailed_tabs")
  shinyjs::hide("vault_hint")

  # URL parameter handling, used to automatically select values
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
    
    updateSelectInput(session, "filter_lustrevolume", selected = volume)
    updateTextInput(session, "filter_unixgroup", value = group)
    updateSelectInput(session, "filter_pi", selected = PI)
  })
  
  # Construct the graph step by step based on user input
  assemblePlot <- function() {
    
    if(input$graph_selector == "scatter") {
      
      # Displays no-value error regardless of which column is used, used
      # column is used arbitrarily
      validate(need(filtered_table()$`used`, "No values to plot!"))
      
      volume_plotter <- ggplot() + 
        geom_point(
          filtered_table(),
          mapping = aes(
            x= `last_modified`,
            y= `used`,
            alpha = 0.1
          )
        ) + 
        scale_alpha(guide="none") +
        scale_y_continuous(labels=filesize_format) + 
        xlab("Last Modified (days)") +
        ylab("Used")
        
      # Renders points corresponding to clicked table rows in red
      
      table_selection <- getSelection()[input$ui_volume_table_rows_selected, ]
      
      volume_plotter <- volume_plotter + geom_point(table_selection,
        mapping = aes(x= `last_modified`,
          y= `used`), size = 2, colour = "red")
      
    } else if (input$graph_selector == "histogram") {
      
      volume_plotter <- ggplot(filtered_table(), aes(`last_modified`)) +
        # Histogram bar height is weighted by file size
        geom_histogram(aes(y=cumsum(..count..), weight=`used`), bins= input$histogram_bins) +
        ylab("used") +
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
    
    if(input$filter_lustrevolume != "All"){
      filtered_graph_table <- filter(filtered_graph_table, 
        str_detect(`scratch_disk`, coll(input$filter_lustrevolume, ignore_case = T)))
    }
    
    if(input$filter_pi != "All"){
      filtered_graph_table <- filter(filtered_graph_table,
        str_detect(`pi_name`, coll(input$filter_pi, ignore_case = T)))
    }
    
    if(!is.null(input$filter_unixgroup)){
      if("All" %notin% input$filter_unixgroup){
        filter_table <- tibble("group_name" = input$filter_unixgroup)
        filtered_graph_table <- semi_join(filtered_graph_table, filter_table,
          by="group_name")
      }
    }
    
    from <- parseBytes(input$filter_size_from, input$filter_size_from_unit)
    to <- parseBytes(input$filter_size_to, input$filter_size_to_unit)
    if(from < to){
      filtered_graph_table <- filter(filtered_graph_table, between(`used`, from, to))
    } else {
      filtered_graph_table <- filter(filtered_graph_table, between(`used`, to, from))
    }
    
    filtered_graph_table <- filter(filtered_graph_table,
      between(`last_modified`, input$filter_lastmodified[1], input$filter_lastmodified[2]))
    
    if(input$filter_archived == "No"){
      filtered_graph_table <- filter(filtered_graph_table, `archived` == 0) 
    } else if(input$filter_archived == "Only") {
      filtered_graph_table <- filter(filtered_graph_table, `archived` == 1)
    }
    
    if(input$filter_humgen == "No") {
      filtered_graph_table <- filter(filtered_graph_table, `is_humgen` == 1)
    } else if(input$filter_humgen == "Only") {
      filtered_graph_table <- filter(filtered_graph_table, `is_humgen` == 0)
    }

    if (input$filter_no_green) {
      filtered_graph_table <- filter(filtered_graph_table, `warning` != "GREEN")
    }
    
    return(filtered_graph_table)
  }
 
  observeEvent(input$clear_filters,{
    updateSelectInput(session, "filter_lustrevolume", selected="All")
    updateSelectInput(session, "filter_pi", selected="All")
    updateTextInput(session, "filter_unixgroup", value="")
    updateNumericInput(session, "filter_size_to", value=1000)
    updateSelectInput(session, "filter_size_to_unit", selected="tb")
    updateNumericInput(session, "filter_size_from", value=0)
    updateSelectInput(session, "filter_size_from_unit", selected="tb")
    updateSliderInput(session, "filter_lastmodified", value=c(0, maximum_age))
    updateSelectInput(session, "filter_archived", selected="Yes")
    updateSelectInput(session, "filter_humgen", selected="No")
  })
  
  # Refilter and rerender the table when any of the fitlers change
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
      input$filter_humgen,
      input$filter_no_green), {
        filterTable(volume_table)
      }, ignoreNULL = FALSE
  )
  
  # --------------------------------
  # ----- Usage Overview Tab -----
  
  output$ui_volume_graph <- renderPlot(assemblePlot())

  # -------------------------
  # This code chunk is used to figure out what data points the user last
  # selected, and then renders them to a graph
  
  reactive_select <- reactiveValues(event_flag = "", selection = empty_tibble)

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

  # priority option is used to ensure that event_flag modifying observers execute
  # before the selection picking observer
  observeEvent(input$graph_click, priority = 10, {
    reactive_select[['event_flag']] <- "click"
  })

  observeEvent(input$graph_brush, priority = 10, {
    reactive_select[['event_flag']] <- "brush"
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

  # -----------------------
  # --- Detailed Report Tab ---

  ls_unix_id <- NULL
  ls_volume_id <- NULL

  # Display the graph in the detailed report tab when given a record
  createHistoryGraph <- function(last_selected) {
    withProgress(
      message = "Loading...",
      min = 0,
      max = 0,
      {

      # These have to be separated here, because otherwise it breaks
      # Selecting the most recently selected PI, Group and Volume
      ls_pi_id <- last_selected[["pi_id"]]
      ls_unix_id <<- last_selected[["unix_id"]]
      ls_volume_id <<- last_selected[["volume_id"]]

      ls_pi_name <- pis  %>% filter(pi_id == ls_pi_id)  %>% select("pi_name")  %>% collect()
      ls_unix_name <- unix_groups  %>% filter(group_id == ls_unix_id)  %>% select("group_name")  %>% collect()
      ls_volume_name <- volumes  %>% filter(volume_id == ls_volume_id)  %>% select("scratch_disk")  %>% collect()

      # Get the extra values
      history <- getHistory(connection, list(c(ls_unix_id, ls_volume_id)))
      trends <- createTrend(history)

      # Update the Title
      output$detailed_report_title = renderText({
        paste("Storage Usage | ", ls_unix_name[[1]], " (", ls_pi_name[[1]], ") | ", ls_volume_name[[1]], sep = "")
      })
      output$history_future_title <- renderText("History/Future Predictions")
      output$directories_title <- renderText("Directories")
      output$vault_title <- renderText("HGI Vault Information")

      # Generate the graph
      output$ui_history_graph <- renderPlot({
        ggplot(
          data = history,
          mapping = aes(x = record_date)
        ) +
        ylim(0, max(history$used, history$quota)) +
        xlab("Date") +
        ylab("Storage (GiB)") +
        geom_point(aes(y = used, color = "Used")) +
        geom_line(aes(y = used, color = "Used", linetype = "Historical"))  +
        
        geom_point(aes(y = quota, color = "Quota")) +
        geom_line(aes(y = quota, color = "Quota", linetype = "Historical")) +

        geom_line(data = trends, aes(y = quota, color = "Quota", linetype = "Prediction")) +
        geom_line(data = trends, aes(y = used, color = "Used", linetype = "Prediction")) +
        labs(color = "Colour", linetype = "Line")
        
      })

      # Storage Usage Warnings
      warning <- calculateWarning(trends)
      if (warning == "RED") {
        output$red_warning = renderText({"WARNING - You are very quickly approaching your storage quota"})
        output$amber_warning = NULL
        output$warning_detail = renderText({
          paste(
            "You are currently at ",
            round(trends$used[[1]] * 100/ trends$quota[[1]], digits = 0),
            "% of your storage quota, and are predicted to reach ",
            round(trends$used[[3]] * 100/trends$quota[[1]], digits = 0),
            "% within three days. Urgently review the data being stored, or your capacity requirements.",
            sep = ""
          )
        })
      } else if (warning == "ORANGE") {
        output$amber_warning = renderText({"WARNING - You are approaching your storage quota"})
        output$red_warning = NULL
        output$warning_detail = renderText({
          paste(
            "You are currently at ",
            round(trends$used[[1]] * 100/ trends$quota[[1]], digits = 0),
            "% of your storage quota, and are predicted to reach ",
            round(trends$used[[3]] * 100/trends$quota[[1]], digits = 0),
            "% within a week.", sep = ""
          )
        })
      } else {
        output$red_warning = NULL
        output$amber_warning = NULL
        output$warning_detail = NULL
      }

      # Hide User Selected Prediction
      output$user_prediction = NULL

      # Get directory information from database, and create table
      directories <- getDirectories(connection, ls_unix_id, ls_volume_id)
      output$directories_table <- renderDT(datatable(
        (directories  %>% select(c("project_name", "directory_path", "num_files", "size", "last_modified", "filetypes"))),
        colnames = c("Project", "Path", "Number of Files", "Size (GiB)", "Last Modified (days)", "File Usage (GiB)"),
        rownames = FALSE,
        options = list(
          pageLength=10,
          searching = FALSE
        ),
        escape = FALSE
      ))

      # Get vault information from database and create table
      vaults <- getVaults(connection, ls_unix_id, ls_volume_id)
      output$vault_table <- renderDT(datatable(
        (vaults  %>% select(c("filepath", "action_name", "file_owner", "size_mib", "last_modified"))),
        colnames = c("File", "Vault Action", "Owner", "Size (MiB)", "Last Modified"),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          searching = FALSE
        )
      ))

      # Show date picker and tabs if hidden
      shinyjs::show("pred_date")
      shinyjs::show("detailed_tabs")
    })
  }

  # Update the detailed report when a record is clicked in the main table
  observeEvent(input$ui_volume_table_rows_selected, {
    dataTableProxy("warnings_summary_table") %>% selectRows(NULL)
    createHistoryGraph(tail(getSelection()[input$ui_volume_table_rows_selected, ], n = 1))
  })
  
  # Custom prediction date picker
  observeEvent(input$pred_date, {
    history <- getHistory(connection, list(c(ls_unix_id, ls_volume_id)))
    prediction <- createPrediction(history, input$pred_date)
    quota <- (history  %>% arrange(desc(record_date)))$quota[[1]]
    usage = round(prediction * 100/ quota, 2)

    output$user_prediction <- renderTable({
      data.frame(
        c("Predicted Usage (GiB)", "Quota (GiB)", "Usage (%)"),
        c(prediction, quota, usage)
      )
    },
    colnames = FALSE
  )}, ignoreInit = TRUE)

  # -----------------------------------
  # --- Main Table at Bottom ---

  # Format the main data into a nice table to be displayed
  formatTable <- function() {
    orig <- getSelection()
    return(
      datatable(
        (orig  %>% select("pi_name", "group_name", "scratch_disk", "is_humgen_yn", "used_gib", "quota_gib", "quota_use", "last_modified", "archived_yn", "warning")),
        colnames = c("PI", "Group", "Disk", "HumGen?", "Used (GiB)", "Quota (GiB)", "Usage (%)", "Last Modified (days)", "Archived?", "Status"),
        rownames = FALSE,
        options = list(
          pageLength=10,
          order = list(list(6, "desc")), # order column 6 [0-indexed] descending (usage)
          searching = FALSE
        ),
        escape = FALSE,
        selection = "single"
      )  %>% 
      formatStyle(
        "warning",
        backgroundColor = styleEqual(c("Not OK", "Kinda OK", "OK"), c("red", "orange", "green"))
      )
    )
  }
  
  output$ui_volume_table <- renderDT(formatTable())
  
  # -------------------------
  # --- Downloads ---
  
  output$downloadFull <- downloadHandler(
    filename = function() {
      # format the filename as 'report-YYYYMMDD.tsv'
      paste("report-", str_replace_all(input$date_picker, '-', ''), ".tsv", sep="")
    },
    content = function(file) {
      # exclude hidden quota_use column from file
      write.table(select(volume_table(), -c(quota_use)),
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

  # -------------------------------
  # User Storage

  observeEvent(input$user_storage_submit, {
    output$ui_user_storage_table <- renderDT(
      getUserUsage(
        connection,
        input$user_storage_filter_user,
        input$user_storage_filter_group,
        input$user_storage_filter_lustrevolume
      ),
      colnames = c("User", "Group", "Volume", "Size (MiB)", "Last Modified"),
      rownames = FALSE,
      options = list(
        pageLength=10,
        searching = FALSE,
        order = list(list(3, "desc")) # order by column 3 [0-indexed] desc (size)
      )
    )

    output$ui_vault_history_table <- renderDT(
      getVaultHistory(
        connection,
        input$user_storage_filter_user,
        input$vault_history_filter_file,
        input$user_storage_filter_lustrevolume,
        input$user_storage_filter_group
      )  %>% select("filepath", "record_date", "action_name"),
      colnames = c("File", "Date", "Vault Action"),
      rownames = FALSE,
      options = list(
        pageLength=10,
        searching = FALSE,
        order = list(list(1, "desc")) # Order Column 1 [0-indexed] (date)
      ),
      escape = FALSE
    )

    output$ui_user_storage_table_title <- renderText("Your Storage Usage")
    output$ui_user_storage_vault_title <- renderText("Your Tracked Files")
    shinyjs::show("vault_hint")

  }, ignoreInit = TRUE)

  # -------------------------------
  
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
    sizeofSelection <- sum(selection$`used`) / 1024**4
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

regenDBData()
shinyApp(
  ui=ui_gen(date_list, blank_dates, volumes, pis, unix_groups, maximum_size, maximum_age),
  server=server
)
