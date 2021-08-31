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

getFileUsage <- function(directory_id_filter, connection) {
    # although the filetypes are unlikely to change
    # we have to ask every time for every row
    # because R won't work otherwise
    filetypes <- tbl(connection, "filetype")  %>% 
        select(c("filetype_id", "filetype_name"))
    
    # Ask the `file_size` DB table for its information for a particular `directory_id`
    file_usage <- tbl(connection, "file_size")  %>% 
        filter(`directory_id` == directory_id_filter)  %>% 
        select(c("filetype_id", "size"))  %>% 
        inner_join(filetypes)  %>% 
        mutate(`size` = round(`size`, digits = 2))  %>% 
        collect()

    # We're going to format this into a string, separating the filetype_name and size by ": ", and each row by "<br>"
    return(paste(file_usage$filetype_name, file_usage$size, sep=": ", collapse="<br>"))

}

getDirectories <- function(connection, group_id_filter, volume_id_filter) {
    # Ask the `directory` DB table for its information by PI and Group
    directories <- tbl(connection, "directory")  %>% 
    filter(`group_id` == group_id_filter)  %>% 
    filter(`volume_id` == volume_id_filter)  %>% 
    select(c("directory_id", "project_name", "directory_path", "num_files", "size", "last_modified"))  %>% 
    collect()

    # If we've got data, we want to add in all the filetype usages (getFileUsage)
    # otherwise, we just need to create the column, but leave it empty
    if (nrow(directories) != 0) {
        directories <- directories %>% rowwise()  %>% 
        mutate("filetypes" = getFileUsage(`directory_id`, connection))
    } else {
        directories <- directories %>% mutate("filetypes" = "")
    }

    return(directories)
}

getDirectoriesByProject <- function(connection, project_name_filter) {
    # Ask the `directory` DB table for its information by PI and Group
    splt = str_split(project_name_filter, pattern = " ")
    filter_project = splt[[1]][1]
    filter_scratch = str_sub(splt[[1]][2], 2, -2)
    
    vol = volumes  %>% filter(`scratch_disk` == filter_scratch)  %>% collect()
    id = vol$volume_id[[1]]
    
    directories <- tbl(connection, "directory")  %>% 
    filter(`project_name` == filter_project)  %>% 
    filter(`volume_id` == id)  %>% 
    select(c("directory_id", "project_name", "directory_path", "num_files", "size", "last_modified"))  %>% 
    collect()

    # If we've got data, we want to add in all the filetype usages (getFileUsage)
    # otherwise, we just need to create the column, but leave it empty
    if (nrow(directories) != 0) {
        directories <- directories %>% rowwise()  %>% 
        mutate("filetypes" = getFileUsage(`directory_id`, connection))
    } else {
        directories <- directories %>% mutate("filetypes" = "")
    }

    return(directories)
}