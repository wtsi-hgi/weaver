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

loadDataByDate <- function(connection, date) {
    return(
        tbl(connection, "lustre_usage") %>% 
        filter(`record_date` == date) %>%
        select(c('used', 'quota', 'archived', 'last_modified', 'pi_id', 'unix_id', 'volume_id')) %>%
        left_join(pis) %>% 
        inner_join(unix_groups, by=c("unix_id" = "group_id")) %>%
        inner_join(volumes)  %>% 
        collect() %>%
        # converts columns imported as int64 to double, they play nicer with the rest of R
        mutate(
            `quota` = as.double(`quota`),
            `used` = as.double(`used`)
            ) %>%
        # creates a quota column
        mutate(
            quota_use = na_if(round(`used` * 100/`quota`, digits = 2), Inf),
            `quota` = na_if(`quota`, 0)
            ) %>%
        mutate("used_gib" = round(readBytes(used, "gb"), digits=2), "quota_gib" = round(readBytes(quota, "gb"), digits = 2))  %>% 
        mutate(is_humgen_yn = ifelse(is_humgen == 1, "Yes", "No"), archived_yn = ifelse(archived == 1, "Yes", "No"))
    )
}