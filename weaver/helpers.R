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

# custom transformation used to simultaneously reverse and logify a graph axis
reverse_log10_trans <- scales::trans_new(
  name = "reverse_log10",
  transform = function(x){ return(-log10(x)) },
  inverse = function(x){ return(10^(-x)) }
);