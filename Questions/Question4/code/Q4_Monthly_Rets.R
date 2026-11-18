Q4_Monthly_Rets <- function(df){

    df %>%
        # label each date with its year and month
        mutate(YM = format(date, "%Y%B")) %>%
        # keep the last trading day of each month
        group_by(Name, YM) %>%
        filter(date == last(date)) %>%
        ungroup() %>%
        arrange(date) %>%
        # monthly return from the month-end index levels
        group_by(Name) %>%
        mutate(Returns = TRI / lag(TRI) - 1) %>%
        ungroup() %>%
        filter(!is.na(Returns)) %>%
        select(date, YM, Name, Returns)
}