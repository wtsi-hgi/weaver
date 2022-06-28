# Copyright (C) 2019, 2021  Genome Research Limited
# Author: 
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

library(DBI)

collapseInner <- function(pair) {
    return(paste("(", paste(pair, collapse=", "), ")", sep=""))
}

formatPairs <- function(filter_pairs) {
    inner <- map(filter_pairs, collapseInner)
    return(paste("(", paste(inner, collapse=", "), ")", sep=""))
}

getHistory <- function(connection, filter_pairs) {

    # The reason this is like this and not using inbuilt filter functions or 
    # prepared statements is that R/dplyr wasn't having it, and I spent hours
    # trying to do filter two columns in dplyr, and it wouldn't do it, 
    # so I just wrote out the (quite simple) SQL. It also doesn't use
    # prepared statements, because that also wouldn't work, R wouldn't pass
    # in the data in the right form. 

    all_history <- dbGetQuery(connection,
        paste("SELECT used, quota, record_date, unix_id, base_directory_id FROM lustre_usage
        WHERE (unix_id, base_directory_id) IN ", formatPairs(filter_pairs), " AND record_date > NOW() - INTERVAL 8 MONTH"
    ))

    return(all_history  %>% mutate(
        used = round(used / (1024**3), digits = 2),
        quota = round(quota / (1024**3), digits = 2)
    ))
}

createPrediction <- function(history, date) {
    ordered <- history  %>% arrange(desc(record_date))
    
    # Ideally a prediction would be based over 3 data points, but there may only be 2 or 1.
    # 2 will still give a prediction, just not very good
    # 1 will just continue the trend assuming nothing changes because its got nothing better to do

    points <- min(nrow(ordered), 3)
    if (points == 1) {
        return(ordered$used[[1]])
    } else {
        prev_1 <- as.numeric(Sys.Date()) - as.numeric(ordered$record_date[[1]])
        prev_2 <- as.numeric(Sys.Date()) - as.numeric(ordered$record_date[[points]]) # Third date for a bit of integrity

        # Number of days from now
        delta <-  as.numeric(date) - as.numeric(Sys.Date())

        # Generate Prediction
        pred = ordered$used[[1]] + ((delta + prev_1)/(prev_2 - prev_1))*(ordered$used[[1]] - ordered$used[[points]])
        return(pred)
    }

}

createTrend <- function(history) {
    ordered <- history  %>% arrange(desc(record_date))
    quota = ordered$quota[[1]]

    # Calculate 3 and 7 days from now in Date format
    day_3 = as.Date(as.numeric(Sys.Date()) + 3, origin = "1970-01-01")
    day_7 = as.Date(as.numeric(Sys.Date()) + 7, origin = "1970-01-01")

    # Estimate 3 and 7 Days from Now
    pred_3 = createPrediction(history, day_3)
    pred_7 = createPrediction(history, day_7)

    return(
        data.frame(
            quota = c(quota, quota, quota),
            used = c(ordered$used[[1]], pred_3, pred_7),
            record_date = c(
                ordered$record_date[[1]],
                day_3,
                day_7
            )
        )
    )
}
