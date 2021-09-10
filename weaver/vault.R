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

getVaults <- function(connection, group_id_filter, volume_id_filter) {
    # This is asking for the vault information given group_id and volume_id

    vaults_query <- dbSendQuery(connection, 
    "SELECT filepath, vault_action_id, size, file_owner, last_modified
    FROM hgi_lustre_usage_new.vault WHERE record_date IN (
        SELECT MAX(record_date) FROM hgi_lustre_usage_new.vault)
    AND volume_id = ? AND group_id = ?")
    dbBind(vaults_query, list(volume_id_filter, group_id_filter))
    vaults <- dbFetch(vaults_query)  %>% 
    inner_join(vault_actions, copy = TRUE)  %>% 
    collect()  %>% 
    mutate(`size` = as.double(`size`))  %>% 
    mutate("size_mib" = round(readBytes(`size`, "mb"), digits = 2))

    return(vaults)
}

getVaultsByProject <- function(connection, project_name_filter) {
    # This is asking for vault information given an entry from the 'Other Data'
    # list. This must first be split up, to get the project and volume

    splt = str_split(project_name_filter, pattern = " ")
    filter_project = splt[[1]][1]
    filter_scratch = str_sub(splt[[1]][2], 2, -2)
    
    vol = volumes  %>% filter(`scratch_disk` == filter_scratch)  %>% collect()
    id = vol$volume_id[[1]]

    vaults_query <- dbSendQuery(connection, 
    "SELECT filepath, vault_action_id, size, file_owner, last_modified
    FROM hgi_lustre_usage_new.vault WHERE record_date IN (
        SELECT MAX(record_date) FROM hgi_lustre_usage_new.vault) 
    AND volume_id = ? AND filepath LIKE ?")
    vaults_query <- dbBind(vaults_query, list(id, paste("%", filter_project, "%", sep = "")))
    vaults <- dbFetch(vaults_query)  %>% 
    inner_join(vault_actions, copy = TRUE)  %>% 
    collect()  %>% 
    mutate(`size` = as.double(`size`))  %>% 
    mutate("size_mib" = round(readBytes(`size`, "mb"), digits = 2))

    return(vaults)
}

getVaultHistory <- function(connection, user_filter, file_filter, volume_filter) {
    # This bit is a bit of a bodge, to filter by what we want

    base_query <- "SELECT filepath, record_date, vault_action_id FROM hgi_lustre_usage_new.vault INNER JOIN hgi_lustre_usage_new.volume USING (volume_id)"
    file_filter_query <- "filepath LIKE ?"
    user_filter_query <- "file_owner = ?"
    volume_filter_query <- "scratch_disk = ?"

    filters_to_use <- c()
    filter_values <- list()
    if (file_filter != "") {
        filters_to_use <- append(filters_to_use, file_filter_query)
        filter_values <- append(filter_values, paste("%", file_filter, "%", sep = ""))
    }
    if (user_filter != "") {
        filters_to_use <- append(filters_to_use, user_filter_query)
        filter_values <- append(filter_values, user_filter)
    }
    if (volume_filter != "All") {
        filters_to_use <- append(filters_to_use, volume_filter_query)
        filter_values <- append(filter_values, volume_filter)
    }
    if (length(filters_to_use) != 0) {
        base_query <- paste(base_query, "WHERE", paste(filters_to_use, collapse=" AND "))

        results_query <- dbSendQuery(connection, base_query)
        results_query <- dbBind(results_query, filter_values)

        results <- dbFetch(results_query)
    } else {
        results <- dbGetQuery(connection, base_query)
    }

    results <- results %>% 
    inner_join(vault_actions, copy = TRUE)  %>% 
    collect()
    return(results)

}