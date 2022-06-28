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

library(DBI)

loadDBData <- function(connection) {
  volumes <- dbGetQuery(
    connection, 
    glue::glue(
      "
      SELECT 
          volume_id, MAX(record_date) AS record_date
      FROM
          lustre_usage
              LEFT JOIN
          base_directory USING (base_directory_id)
      GROUP BY volume_id
      "
    )
  )
  params <- glue::glue("({volumes$volume_id}, '{volumes$record_date}')") %>% 
    paste(collapse = ", ")

  query <- glue::glue(
    "
      SELECT 
        used, 
        quota, 
        last_modified, 
        pi_id, 
        unix_id, 
        volume_id, 
        base_directory_id,
        directory_path,
        record_date, 
        warning_id,
        scratch_disk
      FROM {conf$database}.lustre_usage 
      LEFT JOIN base_directory 
        USING (base_directory_id)  
      INNER JOIN volume 
        USING (volume_id)
      WHERE (volume_id, record_date) IN ({params})
      " 
  )
  dbGetQuery(connection, query) %>% 
    left_join(pis, copy = TRUE)  %>% 
    inner_join(unix_groups, by = c("unix_id" = "group_id"), copy = TRUE) %>%
    inner_join(volumes, copy = TRUE)  %>% 
    inner_join(warning_levels, copy = TRUE)  %>% 
    collect() %>%
    # converts columns imported as int64 to double, they play nicer with the rest of R
    mutate(
      quota = as.double(quota),
      used = as.double(used)
    ) %>% 
    mutate(
      used_gib = round(readBytes(used, "gb"), digits = 2), 
      quota_gib = round(readBytes(quota, "gb"), digits = 2)
    ) %>% 
    # creates a quota column
    mutate(
      quota_use = na_if(round(used_gib * 100/quota_gib, digits = 2), Inf),
      quota_gib = na_if(quota_gib, 0)
    ) %>% 
    rowwise() %>%
    mutate(path = getBaseDirectory(directory_path, scratch_disk))
}

loadScratchDates <- function(connection) {
  dbGetQuery(
    connection,
    glue::glue(
      "
      SELECT 
          CONCAT('Volumes: ',
                  GROUP_CONCAT(volume
                      SEPARATOR ', ')) AS volumes,
          record_date
      FROM
          (SELECT 
              volume_id,
                  RIGHT(scratch_disk, 3) AS volume,
                  MAX(record_date) AS record_date
          FROM
              {conf$database}.lustre_usage
          INNER JOIN {conf$database}.base_directory USING (base_directory_id)
          INNER JOIN {conf$database}.volume USING (volume_id)
          GROUP BY volume_id) AS max_dates
      GROUP BY record_date
      ORDER BY record_date DESC   
      "
    )
  ) %>% 
    collect()  %>% 
    mutate(record_date = format(record_date, "%d/%m/%Y"))
}
