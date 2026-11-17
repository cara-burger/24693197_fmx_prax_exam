Q1_SafeCore_Returns <- function(Holdings, Stocks, Name = "SafeCore"){

    # Stock returns are only available from 2015, so use holdings from then on
    Holdings <- Holdings %>% filter(date >= min(Stocks$date))

    wts <- Holdings %>% tbl_xts(cols_to_xts = Weight, spread_by = Tickers)

    # Returns of the stocks held, from the first holdings date
    rts <- Stocks %>% filter(Tickers %in% colnames(wts), date > min(Holdings$date)) %>%
        tbl_xts(cols_to_xts = Returns, spread_by = Tickers)

    # Stocks not held, or without a return on a day, get zero
    wts[is.na(wts)] <- 0

    rts[is.na(rts)] <- 0

    rmsfuns::Safe_Return.portfolio(R = rts, weights = wts, lag_weights = T) %>%
        xts_tbl() %>%
        rename(Returns = portfolio.returns) %>%
        mutate(Tickers = Name, Name = Name)

}