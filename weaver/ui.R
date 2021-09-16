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
                column(8,
                    titlePanel("Weaver"),
                    h4("Lustre Usage Reports"),
                    br()
                ),
                column(4,
                    br(),
                    strong("Latest Data"),
                    br(),
                    tableOutput("result_dates")
                )
            ),
            tabsetPanel(
                tabPanel("Home/Help",
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
                            p("Vault removes files that haven't been modified recently, although you can force it to either
                            keep a file, or archive it in iRODS. This will show the files that have been given
                            a specific tag for Vault to read."),
                            a(href = "https://confluence.sanger.ac.uk/display/HGI/Data+Management+Policy", target="_blank", "Vault on Confluence")
                        )
                    ),
                    strong("Warnings"),
                    p("By selecting a PI or Lustre Volume using the filters on the left (under 'Data'), you'll be presented
                        with an overview of the appropriate records, their storage usage and their warning state. You can
                        choose to view only non-green statuses."),
                    p("Selecting a row here will load it into the 'Detailed Report' tab if you wish to view more there."),
                    a(href = "https://confluence.sanger.ac.uk/pages/viewpage.action?pageId=28646257", target="_blank", "More on Confluence"),
                    br(), br(),
                    strong("This data isn't real time, and can often be a few days out of date."),
                    p("The dates that the information comes from per volume is at the bottom of the page.")
                ),
                tabPanel("View by User",
                    br(),
                    fluidRow(
                        column(4,
                            selectInput(
                                "user_storage_filter_lustrevolume",
                                "Lustre Volume",
                                choices = c("All", as.list(volumes  %>% select("scratch_disk")  %>% collect())), 
                                selected="All"
                            ),
                            textInput(
                                "user_storage_filter_user",
                                "Username"
                            ),
                            textInput(
                                "user_storage_filter_group",
                                "Unix Group"
                            ),
                            textInput(
                                "vault_history_filter_file",
                                "Filename (for Vault History)"
                            ),
                            actionButton(
                                "user_storage_submit",
                                label="Submit"
                            ),
                            br(), br()
                        ),
                        column(8,
                            textOutput("ui_user_storage_table_title", container = h3),
                            DTOutput("ui_user_storage_table"),
                            textOutput("ui_user_storage_vault_title", container = h3),
                            DTOutput("ui_vault_history_table")
                        )
                    )
                ),
                tabPanel("View by Group",
                    br(),
                    fluidRow(
                        column(4,
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
                            tags$strong("Volume Size Range"),
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
                            selectInput("filter_archived", "Show Archived Directories?",
                                choices = list("Yes", "No", "Only"), selected = "Yes"
                            ),
                            selectInput("filter_humgen", "Show Non-HumGen Groups?",
                                choices = list("Yes", "No", "Only"), selected = "No"
                            ),
                            actionButton("clear_filters", "Clear Filters"),
                            br(),
                            br()
                        ),
                        column(8,
                            plotOutput("ui_volume_graph",
                                click = "graph_click",
                                brush = brushOpts(id = "graph_brush", resetOnNew=FALSE)
                            ),
                            textOutput("ui_selection_size"),
                            br(), 
                            fluidRow(
                                column(6,
                                    strong("Axes to Scale Logarithmically"),
                                    checkboxInput("log_x", "Last Modified", value=FALSE),
                                    # Don't show y-axis logifier in histogram mode, it freaks out at values <1
                                    conditionalPanel("input.graph_selector == 'scatter'",
                                        checkboxInput("log_y", "Volume Size", value=FALSE)
                                    )
                                ),
                                column(6,
                                    radioButtons("graph_selector", 
                                        strong("Graph Type"),
                                        choices = list("Scatter" = "scatter", "Cumulative Histogram" = "histogram"),
                                        selected = "scatter"
                                    ),
                                
                                    conditionalPanel("input.graph_selector == 'histogram'",
                                        numericInput("histogram_bins", h4("Histogram bin count"), value=40)
                                    )
                                )
                            )
                        )
                    ),
                    hr(style="border-color:black;"),
                    DTOutput("ui_volume_table"),
                    actionButton("clear_full", "Clear Selection"),
                    downloadButton("downloadFull", "Download Report"),
                    downloadButton("downloadTable", "Download Table"),
                    br(), br()
                    hr(style="border-color:black;"),
                    
                )
            ),
            fluidRow(

                column(8,
                tabsetPanel(
                    tabPanel("Detailed Report",
                        textOutput("detailed_report_title", container = h4),
                        tabsetPanel(
                            tabPanel("History/Future Predictions",
                                br(),
                                textOutput("no_history_warning"),
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
                            tabPanel("HGI Vault Information",
                                br(),
                                DTOutput("vault_table")
                            ),
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
            
        )
    )
}