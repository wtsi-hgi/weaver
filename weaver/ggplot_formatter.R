require(tidyverse)

# Nicked wholesale from fdryan's R repository https://github.com/fdryan/R but
# repurposed to produce file size formatting instead

#' Human Numbers: Format numbers so they're legible for humans
#' Use this in ggplot for labels where you might use the comma or percent functions from the 
#' Scales package.
#' 
#' Checks whether numbers are positive or negative. 
#' Allows up to 1 significant figure
#' sapply used for element-wise application of the humanity function as a vector may include
#' numbers where billions, millions or thousands are appropriate.
#'
#' @return a character vector the same length as the input vector
#' @param x a numeric vector to format, 
#' @smbl a symbol you'd like to prefix your numbers by e.g. "$"
#' @signif = the number of significant places you want the function to return
#' @examples
#' human_numbers(c(1000000 , 1500000, 10000000000))
#' human_numbers(c(1.200000e+05, -2.154660e+05, 2.387790e+05, 4.343500e+04 ,5.648675e+12), "$")
#' ggplot2 + scale_y_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_numbers)
#' ggplot2 + scale_x_continuous(labels = human_gbp)

filesize_format <- function(x = NULL, smbl ="", signif = 1){
  humanity <- function(y){
    
    if (!is.na(y)){
      p <- round(abs(y) / 1024**5, signif)
      tn <- round(abs(y) / 1024**4, signif)
      b <- round(abs(y) / 1024**3, signif)
      m <- round(abs(y) / 1024**2, signif)
      k <- round(abs(y) / 1024, signif)
      
      if ( y >= 0 ){
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "KiB")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"MiB")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"GiB")
      } else if(p < 1){
        paste0 (y_is_positive, smbl,  comma(tn), "TiB")
      } else {
        paste0 (y_is_positive, smbl, comma(p), "PiB")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}
