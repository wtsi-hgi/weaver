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

library(tidyverse)
library(DT)
library(scales)

source("predictions.R")

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

readBytes <- function(size, extension) {
  if(extension == "tb") {
    return(size/1024**4)
  } else if(extension == "gb") {
    return(size/1024**3)
  } else if(extension == "mb") {
    return(size/1024**2)
  } else if(extension == "kb") {
    return(size/1024)
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

# Nicely format the table from warningsTableDate() below to be displayed in Shiny
formatWarningsTable <- function(full_table, db, no_green, filter_pi, filter_volume, session) {
  marked_data <- warningsTableData(full_table, db, no_green, filter_pi, filter_volume, session)
  if(nrow(marked_data) != 0) {
    return(
      datatable(
        (marked_data  %>% select("group_name", "pi_name", "scratch_disk", "quota_use", "last_modified", "warning")),
        colnames = c("Group", "PI", "Disk", "Usage (%)", "Last Modified (days)", "Status"),
        rownames = FALSE,
        selection = "single",
        options = list(
          order = list(list(5, "asc")), # Order Column 5 [0-indexed] (status)
          searching = FALSE,
          escape = FALSE
        )
      )
    )
  }

  return(NULL)
}


getWarningTable <- function(no_green, session) {
  marked_data <- session$userData$warningsTableCache
  
  if (no_green) {
    marked_data = marked_data  %>% filter(`warning` != "🟢")
  }


  return(marked_data)
}

# Calculate the table of warnings for a PI/Lustre Volume
warningsTableData <- function(full_table, db, no_green, filter_pi, filter_volume, session) {
  
  # Uses a cache if it can instead of recalculating everything
  if (
    !is.null(session$userData$filter_pi_cache) &&
    !is.null(session$userData$filter_volume_cache) &&
    filter_pi == session$userData$filter_pi_cache && 
    filter_volume == session$userData$filter_volume_cache
  ) {
    return(getWarningTable(no_green, session))
  } 
  
  filter_pairs <- list()
  for (row in 1:nrow(full_table)) {
    data <- full_table[row,]
    filter_pairs[[row]] <- c(data[["unix_id"]], data[["volume_id"]])
  }

  history <- getHistory(db, filter_pairs)

  warnings <- c()
  for (row in 1:nrow(full_table)) {
    data <- full_table[row,]
    row_history <- history  %>% filter(unix_id == data[["unix_id"]])  %>% filter(volume_id == data[["volume_id"]])  %>% collect()
    warning <- calculateWarning(createTrend(row_history))
    if (warning == "RED") {
      symbol <- "🔴"
    } else if (warning == "AMBER") {
      symbol <- "🟡"
    } else {
      symbol <- "🟢"
    }
    warnings <- append(warnings, symbol)
  }

  marked_data <- full_table  %>% select("group_name", "pi_name", "scratch_disk", "quota_use", "last_modified", "pi_id", "unix_id", "volume_id") %>% mutate("warning" = warnings)
  
  session$userData$warningsTableCache <- marked_data
  session$userData$filter_pi_cache <- filter_pi
  session$userData$filter_volume_cache <- filter_volume

  if (no_green) {
    marked_data = marked_data  %>% filter(`warning` != "🟢")
  }

  return(marked_data)

}