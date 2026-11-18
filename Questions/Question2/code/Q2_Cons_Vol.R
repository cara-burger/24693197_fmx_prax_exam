# Weighted average rolling realised volatility of index constituents, at each month end
Q2_Cons_Vol <- function(Stocks, Size_Clean, Window = 252){

    # Rolling annualised SD per stock, kept on the last trading day of each month
    Stock_SD <-
        Rolling_Vol(Stocks, Window = Window) %>%
        mutate(YearMonth = format(date, "%Y%B")) %>%
        group_by(Tickers, YearMonth) %>%
        filter(date == last(date)) %>%
        ungroup() %>%
        select(Tickers, YearMonth, RollSD)

    # Weighted average constituent SD per index per month
    left_join(
        Size_Clean %>% mutate(YearMonth = format(date, "%Y%B")),
        Stock_SD,
        by = c("Tickers", "YearMonth")
    ) %>%
        filter(!is.na(RollSD)) %>%
        group_by(Fund, date) %>%
        summarise(RollSD = sum(Weight * RollSD) / sum(Weight))
}