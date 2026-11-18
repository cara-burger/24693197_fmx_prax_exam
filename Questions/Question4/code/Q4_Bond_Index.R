Q4_Bond_Index <- function(Yields, Mod_Duration = 9){

    Yields %>%
        # the TRI column holds yields in percent
        rename(Yield = TRI) %>%
        # keep the yield on the last trading day of each month
        mutate(YM = format(date, "%Y%B")) %>%
        group_by(Name, YM) %>%
        filter(date == last(date)) %>%
        ungroup() %>%
        arrange(date) %>%
        group_by(Name) %>%
        # yield in decimals
        mutate(i = Yield / 100,
               # change in yield over the month
               Delta_i = i - lag(i),
               # price change is minus modified duration times the change in yield
               Price_Chg = -Mod_Duration * Delta_i,
               # interest earned over the month is the annual yield split over twelve months
               Income = lag(i) / 12,
               # convexity is left out as monthly yield changes are small (this is a deliberate simplification in the interest of time, can be revisted)
               Returns = Income + Price_Chg) %>%
        ungroup() %>%
        # first month has no previous yield so it is dropped
        filter(!is.na(Returns)) %>%
        select(date, YM, Name, Returns)
}