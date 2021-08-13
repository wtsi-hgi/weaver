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

ui_gen <- function(date_list, blank_dates, volumes, pis, unix_groups, maximum_size, maximum_age) {
    return(
        fluidPage(

            useShinyjs(),

            fluidRow(
                column(4,
                titlePanel("Weaver"),
                h4("Lustre Usage Reports"),
                br()
                ),
                column(4,
                br(),
                dateInput("date_picker", NULL, value = date_list[[1]],
                    min = date_list[[length(date_list)]],
                    max = date_list[[1]],
                    datesdisabled = blank_dates
                )
                )
            ),
            fluidRow(
                # Left hand side, top panel
                column(4,
                tabsetPanel(
                    tabPanel("Data",
                    h4("Data filters"),
                    selectInput(
                        "filter_lustrevolume",
                        "Lustre Volume",
                        choices = c("All", as.list(volumes  %>% select("scratch_disk")  %>% collect())), 
                        selected="All"
                    ),
                    selectInput(
                        "filter_pi",
                        "PI",
                        choices = c("All", as.list(pis  %>% select("pi_name")  %>% collect())),
                        selected="All"
                    ),
                    selectizeInput(
                        "filter_unixgroup",
                        "Unix Group",
                        choices = c("All", as.list(unix_groups  %>% select("group_name")  %>% collect())),
                        selected=NULL,
                        multiple=TRUE,
                        options = list(create=FALSE)
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
                        min=0, max=maximum_age, value=c(0, maximum_age), step=50
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
                ), #Tabset panel end
                tags$a(href = "/spaceman", "Directory archival form")
                ), # Left hand side top panel end
                
                column(8,
                tabsetPanel(
                    tabPanel("Usage Overview",
                    plotOutput("ui_volume_graph",
                        click = "graph_click",
                        brush = brushOpts(id = "graph_brush", resetOnNew=FALSE)
                    ),
                    textOutput("ui_selection_size")
                    ),
                    tabPanel("Detailed Report",
                    br(),
                    textOutput("history_warning"),
                    plotOutput("ui_history_graph"),
                    span(textOutput("red_warning"), style = "color: red"),
                    span(textOutput("amber_warning"), style = "color: orange"),
                    textOutput("warning_detail"),
                    br(),
                    dateInput(
                        "pred_date",
                        "Select a date for a storage use prediction",
                        min = Sys.Date()
                    ),
                    tableOutput("user_prediction")
                    ),
                    tabPanel("Warnings",
                    h4("Warnings"),
                    textOutput("warnings_summary_name"),
                    checkboxInput(
                        "warnings_no_green",
                        label = "Only display non-green statuses"
                    ),
                    textOutput("no_warnings"),
                    DTOutput("warnings_summary_table")
                    )
                ),
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
    )
}