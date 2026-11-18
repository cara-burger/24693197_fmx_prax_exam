Q3_Rel_12m <- function(Funds, BM_Monthly, Fund = "ABC_Fund") {

    Funds %>%
        left_join(BM_Monthly, by = "YM") %>%
        filter(!is.na(BM_Monthly)) %>%
        arrange(date) %>%
        group_by(Name) %>%
        # Rolling 12 month return of each fund and the benchmark
        mutate(RollRet = RcppRoll::roll_prod(1+Returns, n = 12, fill = NA, align = "right"),
               RollBM = RcppRoll::roll_prod(1+BM_Monthly, n = 12, fill = NA, align = "right")) %>%
        ungroup() %>%
        filter(!is.na(RollRet)) %>% mutate(XS = RollRet - RollBM) %>%
        # Chosen fund against the average peer each month
        mutate(Group = ifelse(Name == Fund, "ABC Fund", "Average peer")) %>%
        group_by(YM, Group) %>%
        summarise(XS = mean(XS))
}