createTrend <- function(history) {
    ordered <- history  %>% arrange(desc(record_date))
    quota = ordered$quota[[1]]

    prev_1 <- as.numeric(Sys.Date()) - as.numeric(ordered$record_date[[1]])
    prev_2 <- as.numeric(Sys.Date()) - as.numeric(ordered$record_date[[3]]) # Third date for a bit of integrity

    # Estimate 3 and 7 Days from Now
    pred_3 = ordered$used[[1]] + ((3 + prev_1)/(prev_2 - prev_1))*(ordered$used[[1]] - ordered$used[[3]])
    pred_7 = ordered$used[[1]] + ((7 + prev_1)/(prev_2 - prev_1))*(ordered$used[[1]] - ordered$used[[3]])

    print(pred_3)
    print(pred_7)

    return(
        data.frame(
            quota = c(quota, quota),
            used = c(pred_3, pred_7),
            record_date = c("2021-08-10", "2021-08-11")
        )
    )
}