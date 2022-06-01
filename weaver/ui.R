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

source("version.R")

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
                    fluidRow(
                        column(6,
                            h3("Welcome to Weaver"),
                            p("This is a tool where you can view your or your groups storage usage on Lustre."),
                            p("Feel free to ask HGI if you have any questions."),
                            strong("View by User"),
                            p("Here, you can filter to find where your files are on Lustre and how much space they're taking up.
                            If you want to filter these by a particular group or volume, you can do that too."),
                            p("You can also see the history of your files in any Vaults. It is very beneficial to use the filename
                            filter here. Please remember that this isn't live information, so you can always use Vault on the farm."),
                            strong("View By Group"),
                            p("Here, you can get more information about a particular groups usage in more detail."),
                            p("The filters on the left are more detailed, and populate both the table at the bottom and the 
                            scatter plot to the right. The plot can be modified with the controls below."),
                            p("The table displays usage information about each group per volume. The status column is based
                            on a prediction of your usage in the next week based on the previous usage. This is combined with
                            your quota to calculate a warning status. There is a filter to only display non-OK statuses."),
                            p("By selecting a row in the table, you can view even more detailed information below."),
                            p("First is a graph plotting your usage and quota over the last eight months, and a dotted line
                            giving a prediction over the next week. Please remember, this is only based on a simple extrapolation."),
                            p("The date picker allows you to pick a date in the future to predict your usage at that date."),
                            p("Below that is a table giving you detail about subdirectories, including storage used by different
                            common file types"),
                            p("Finally is the table giving information about the Vault tracked files in your group. Please
                            remember that this isn't live information"),
                            a(href = "https://confluence.sanger.ac.uk/display/HGI/Vault+Reference+Manual+for+Users", target="_blank", "Vault on Confluence (user guide)"),
                            br(),
                            a(href = "https://confluence.sanger.ac.uk/pages/viewpage.action?pageId=28646257", target="_blank", "Weaver on Confluence (technical guide)"),
                            br(), br(),
                            strong("This data isn't real time, and can often be a few days out of date."),
                            p("The dates that the information comes from per volume is at the top of the page.")
                        )
                    )
                ),
                tabPanel("View by User",
                    # br(),
                    fluidRow(
                        column(4,
                            br(),
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
                            downloadButton("download_user_storage", "Downlaod"),
                            textOutput("ui_user_storage_vault_title", container = h3),
                            DTOutput("ui_vault_history_table"),
                            em("Hint: Use the filename filter", id="vault_hint"),
                            br(),
                            downloadButton("download_user_vaults", "Downlaod"),
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
                            checkboxInput(
                                "filter_no_green",
                                label = "Only display non-OK statuses"
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
                    downloadButton("downloadFull", "Download Unfiltered Table"),
                    downloadButton("downloadTable", "Download Filtered Table"),
                    br(), br(),
                    hr(style="border-color:black;"),
                    
                    textOutput("please_select"),
                    textOutput("detailed_report_title", container = h3),
                    textOutput("history_future_title", container = h4),
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
                    tableOutput("user_prediction"),

                    textOutput("directories_title", container = h4),
                    DTOutput("directories_table"),
                    downloadButton("downloadDirectories", "Download"),

                    textOutput("vault_title", container = h4),
                    DTOutput("vault_table"),
                    downloadButton("downloadVault", "Download")


                )
            ),
            hr(),
            p(paste("HGI Weaver: Version", WEAVER_VERSION, "| Database:", conf$database))
        )
    )
}
