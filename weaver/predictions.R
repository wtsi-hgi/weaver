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

getHistory <- function(connection, ls_pi_id, ls_unix_id, ls_volume_id) {
    return(tbl(connection, "lustre_usage")  %>% 
      filter(pi_id == ls_pi_id)  %>% 
      filter(unix_id == ls_unix_id)  %>% 
      filter(volume_id == ls_volume_id)  %>% 
      select(c("used", "quota", "record_date"))  %>% 
      mutate(used = round(used / 1e+9, digits=2), quota = round(quota / 1e+9), digits = 2)  %>% 
      collect()
    )
}

createTrend <- function(history) {
    # TODO: Needs enough data to do the prediction
    # Throws an error otherwise

    ordered <- history  %>% arrange(desc(record_date))
    quota = ordered$quota[[1]]

    prev_1 <- as.numeric(Sys.Date()) - as.numeric(ordered$record_date[[1]])
    prev_2 <- as.numeric(Sys.Date()) - as.numeric(ordered$record_date[[3]]) # Third date for a bit of integrity

    # Estimate 3 and 7 Days from Now
    pred_3 = ordered$used[[1]] + ((3 + prev_1)/(prev_2 - prev_1))*(ordered$used[[1]] - ordered$used[[3]])
    pred_7 = ordered$used[[1]] + ((7 + prev_1)/(prev_2 - prev_1))*(ordered$used[[1]] - ordered$used[[3]])

    return(
        data.frame(
            quota = c(quota, quota, quota),
            used = c(ordered$used[[1]], pred_3, pred_7),
            record_date = c(
                ordered$record_date[[1]],
                as.Date(as.numeric(Sys.Date()) + 3, origin = "1970-01-01"),
                as.Date(as.numeric(Sys.Date()) + 7, origin = "1970-01-01")
            )
        )
    )
}

calculateWarning <- function(trends) {
    quota = trends$quota[[1]]
    if (!is.na(quota) && quota != 0) {
        day3 = trends$used[[2]] / quota
        day7 = trends$used[[3]] / quota
    } else {
        day3 = 0
        day7 = 0
    }

    if (day3 > 0.9 || day7 > 0.95) {
        return("RED")
    } else if (day7 > 0.9) {
        return("AMBER")
    } else {
        return("")
    }
}