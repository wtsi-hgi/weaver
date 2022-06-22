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

getDirectories <- function(connection, group_id_filter, base_directory_id_filter) {
    directories_query <- dbSendQuery(
      connection,
      "
      SELECT 
          volume.scratch_disk as volume,
          base_directory.directory_path AS base_directory_path,
          directory.directory_id,
          directory.base_directory_id,
          directory.directory_path,
          num_files,
          size,
          last_modified,
          (SELECT 
                  GROUP_CONCAT(CONCAT(filetype_name, ': ', size)
                          SEPARATOR ', ')
              FROM
                  file_size
                      INNER JOIN
                  filetype USING (filetype_id)
              WHERE
                  file_size.directory_id = directory.directory_id) AS filetypes
      FROM
          directory
      INNER JOIN
          base_directory USING (base_directory_id)
      INNER JOIN
        volume USING (volume_id)
      WHERE base_directory_id = ? 
        AND group_id = ?
      "
    )
    directories_query %>% 
      dbBind(list(base_directory_id_filter, group_id_filter)) %>% 
      dbFetch() %>%
      rowwise() %>% 
      mutate(
        num_files = as.double(num_files),
		    size = as.double(size),
		    project = fs::path_rel(base_directory_path, fs::path("/lustre", volume))
      )
}
