Q4_Port_Returns <- function(Weights, Returns, Name){

    # weights at each rebalance date, one column per asset
    wts <- Weights %>% tbl_xts(cols_to_xts = Weight, spread_by = Tickers)

    # monthly asset returns after the first rebalance date
    rts <- Returns %>% filter(Tickers %in% colnames(wts), date > min(Weights$date)) %>%
        tbl_xts(cols_to_xts = Returns, spread_by = Tickers)

    # same column order for returns and weights
    rts <- rts[, names(wts)]

    # assets not held get a zero weight
    wts[is.na(wts)] <- 0

    rts[is.na(rts)] <- 0

    rmsfuns::Safe_Return.portfolio(R = rts, weights = wts, lag_weights = T) %>%
        xts_tbl() %>%
        rename(Returns = portfolio.returns) %>%
        mutate(Tickers = Name)
}