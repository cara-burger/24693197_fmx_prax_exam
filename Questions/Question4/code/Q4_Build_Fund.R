Q4_Build_Fund <- function(MSCI, Bond_Yields, Commods, Idx_Daily, LCL_Yield, ref){

    # global equity and commodities from index levels
    Gl_Rets <- bind_rows(MSCI, Commods) %>%
        filter(Name %in% ref$Name) %>%
        select(date, Name, TRI) %>%
        Q4_Monthly_Rets()

    # global bonds from yields
    Bond_Rets <- Bond_Yields %>%
        filter(Name %in% ref$Name) %>%
        Q4_Bond_Index()

    # local indexes from daily returns
    Lcl_Rets <- Idx_Daily %>%
        #  missing returns are removed so that no zero returns are added before an index starts
        filter(Tickers %in% ref$Name, !is.na(Returns)) %>%
        arrange(date) %>%
        group_by(Tickers) %>%
        mutate(TRI = cumprod(1 + Returns)) %>%
        ungroup() %>%
        select(date, Name = Tickers, TRI) %>%
        Q4_Monthly_Rets()

    # local bond and property indices are already monthly
    Yield_Rets <- LCL_Yield %>%
        filter(Tickers %in% ref$Name) %>%
        select(date, Name = Tickers, Returns)

    # common calendar month-end date (afterwardsadd class and region)
    bind_rows(Gl_Rets, Bond_Rets, Lcl_Rets, Yield_Rets %>% mutate(YM = format(date, "%Y%B"))) %>%
        # use one month-end date per month so that all assets line up
        group_by(YM) %>%
        mutate(date = max(date)) %>%
        ungroup() %>%
        select(date, Name, Returns) %>%
        left_join(ref, by = "Name")
}