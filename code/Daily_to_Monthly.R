# Convert daily returns to monthly returns per ticker
# Chain daily returns into an index, keep the last trading day of each month and take month-on-month returns
Daily_to_Monthly <- function(df){

    df %>%
        arrange(date) %>%
        group_by(Tickers) %>%
        mutate(Index = cumprod(1 + Returns)) %>%
        mutate(YM = format(date, "%Y%B")) %>%
        group_by(Tickers, YM) %>%
        filter(date == last(date)) %>%
        group_by(Tickers) %>%
        mutate(Ret = Index / lag(Index) - 1) %>%
        ungroup() %>%
        filter(!is.na(Ret)) %>%
        select(date, YM, Tickers, Ret)
}