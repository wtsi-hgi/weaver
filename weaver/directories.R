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

getDirectories <- function(connection, group_id_filter, volume_id_filter) {
    directories_query <- dbSendQuery(connection, "
    SELECT directory_id, project_name, directory_path, num_files, size, last_modified,
       (SELECT GROUP_CONCAT(CONCAT(filetype_name, ': ', size) SEPARATOR ', ') FROM file_size
            INNER JOIN filetype USING (filetype_id)
            WHERE file_size.directory_id = directory.directory_id
        ) AS filetypes
        FROM directory 
        WHERE volume_id = ? 
        AND group_id = ?"
    )

    directories_query <- dbBind(directories_query, list(volume_id_filter, group_id_filter))
    return(dbFetch(directories_query) %>%
	   mutate(
		`num_files` = as.double(`num_files`),
		`size` = as.double(`size`)
		 )
	  )
}
