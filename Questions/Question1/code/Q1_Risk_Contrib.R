Q1_Risk_Contrib <- function(Holdings, Stocks, Months = 12){

    # Start date of the window
    Start <- fmxdat::safe_month_min(max(Holdings$date), N = Months)

    # Weights over the window in wide format
    wts <- Holdings %>%
        filter(date >= Start) %>%
        tbl_xts(cols_to_xts = Weight, spread_by = Tickers)

    # Daily returns of the same stocks in wide format
    rts <- Stocks %>%
        filter(Tickers %in% colnames(wts), date > Start) %>%
        tbl_xts(cols_to_xts = Returns, spread_by = Tickers)

    # Set missing weights and returns to zero
    wts[is.na(wts)] <- 0
    rts[is.na(rts)] <- 0

    # Build the portfolio and keep the daily weights
    RetPort <- rmsfuns::Safe_Return.portfolio(rts,
                        weights = wts, lag_weights = TRUE,
                        verbose = TRUE, contribution = TRUE,
                        value = 1000, geometric = TRUE)

    # Average weight of each stock over the window
    wts_avg <- RetPort$BOP.Weight %>% xts_tbl %>%
        summarise( across(.cols = -date, .fns = ~mean(., na.rm=T)) ) %>% gather(Type, wt)

    # Each stock's share of the portfolio's standard deviation
    StdDev( R = rts, portfolio_method = "component", weights = wts_avg$wt) %>%
        .$pct_contrib_StdDev %>% data.frame(Risk_Contrib = .) %>%
        tibble::rownames_to_column("Tickers") %>%
        # Add the average weights
        left_join(wts_avg %>% rename(Tickers = Type), by = "Tickers") %>%
        arrange(desc(Risk_Contrib))

}