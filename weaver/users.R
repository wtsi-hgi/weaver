# Copyright (C) 2021  Genome Research Limited
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

getUserUsage <- function(connection, user_filter, group_filter, volume_filter) {
    base_query <- paste("SELECT user.user_name, unix_group.group_name, volume.scratch_disk, size, last_modified FROM ", conf$database, ".user_usage
    INNER JOIN ", conf$database, ".user USING (user_id)
    INNER JOIN ", conf$database, ".unix_group USING (group_id)
    INNER JOIN ", conf$database, ".volume USING (volume_id)
    WHERE record_date IN (SELECT MAX(record_date) FROM ", conf$database, ".user_usage)", sep="")

    user_filter_query <- "AND user_name = ?"
    group_filter_query <- "AND group_name = ? AND is_humgen=1"
    volume_filter_query <- "AND scratch_disk = ?"

    filters_to_use <- c()
    filter_values <- list()

    if (user_filter != "") {
        filters_to_use <- append(filters_to_use, user_filter_query)
        filter_values <- append(filter_values, user_filter)
    }

    if (group_filter != "") {
        filters_to_use <- append(filters_to_use, group_filter_query)
        filter_values <- append(filter_values, group_filter)
    }

    if (volume_filter != "All") {
        filters_to_use <- append(filters_to_use, volume_filter_query)
        filter_values <- append(filter_values, volume_filter)
    }

    if (length(filters_to_use) != 0) {
        base_query <- paste(base_query, paste(filters_to_use, collapse=" "))

        results_query <- dbSendQuery(connection, base_query)
        results_query <- dbBind(results_query, filter_values)

        results <- dbFetch(results_query)
    } else {
        results <- dbGetQuery(connection, base_query)
    }

    results <- results  %>% 
    collect()  %>% 
    mutate(`size` = round(readBytes(as.double(`size`), "mb"), digits = 2))  %>%  
    mutate(`last_modified` = format(`last_modified`, "%d/%m/%Y"))
    return(results)
}