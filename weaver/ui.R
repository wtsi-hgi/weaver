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
            ),
            fluidRow(
                # Left hand side, top panel
                column(4,
                tabsetPanel(
                    tabPanel("Data",
                    h4("Data Filters"),
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
                    br(),
                    strong("Usage Overview"),
                    p("Click or click and drag on the graph to select data points. Your selection will
                        appear in a table at the bottom of the page. Click on a blank space to clear the
                        selection."),
                    p("Click on a row within a table to highlight the corresponding
                        data point in red on the graph."),
                    strong("Detailed Report"),
                    p("Having selected a row, you can view more information about that data, split between three tabs"),
                    tags$ul(
                        tags$li(
                            strong("Future Predictions"),
                            p("Here, you can view the history of that data on the graph"),
                            p("If you're approaching the storage quota, you'll also be presented with a warning, and information
                                about your storage usage."),
                            p("You can use the date picker at the bottom to view a prediction of your storage usage at any
                                future date. Remember, this is only a simple extrapolation based on recent usage."),
                        ),
                        tags$li(
                            strong("Directories"),
                            p("Here you can view more detailed information about specific directories in the Lustre file system."),
                            p("This includes the storage used by different common file types")
                        ),
                        tags$li(
                            strong("HGI Vault Information"),
                            p("TODO")
                        )
                    ),
                    strong("Warnings"),
                    p("By selecting a PI or Lustre Volume using the filters on the left (under 'Data'), you'll be presented
                        with an overview of the appropriate records, their storage usage and their warning state. You can
                        choose to view only non-green statuses."),
                    p("Selecting a row here will load it into the 'Detailed Report' tab if you wish to view more there."),
                    a(href = "https://confluence.sanger.ac.uk/pages/viewpage.action?pageId=28646257", target="_blank", "More on Confluence"),
                    br(),
                    strong("This data isn't real time, and can often be a few days out of date.")
                    )
                ), #Tabset panel end
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
                        textOutput("detailed_report_title", container = h4),
                        tabsetPanel(
                            tabPanel("Future Predictions",
                                br(),
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
                            tabPanel("Directories",
                                br(),
                                DTOutput("directories_table")
                            ),
                            tabPanel("HGI Vault Information"),
                            id = "detailed_tabs"
                        ),
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