Q1_Rolling_Vol <- function(df, Window = 63){

    df %>%
        # Sort by date so each window contains consecutive trading days
        arrange(date) %>%
        # Calculate the rolling SD separately for each stock
        group_by(Tickers) %>%
        # SD of the last Window daily returns, ending on each date (align = "right"),
        # multiplied by sqrt(252) to convert daily SD into an annual figure
        mutate(RollSD = RcppRoll::roll_sd(Returns, n = Window, fill = NA, align = "right") * sqrt(252)) %>%
        ungroup() %>%
        # The first Window - 1 dates have too little history (drop NA rows)
        filter(!is.na(RollSD))

}