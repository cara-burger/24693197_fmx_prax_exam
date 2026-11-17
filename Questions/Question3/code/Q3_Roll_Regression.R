Q3_Roll_Regression <- function(Manager_list, BM_Monthly, Nsel = 36, Silent = FALSE) {

    # Uncomment to step through the function for one fund
    # Manager <- Manager_list[[2]]

    Manager <- Manager_list

    # Skip funds with too few months to fill a window (NULL is ignored by map_df)
    if(nrow(Manager) < Nsel) return(NULL)

    # Progress message showing which fund is being estimated
    if(!Silent) message(glue::glue("Starting: {unique(Manager$Name)}"))

    # Split the fund's dates into rolling windows of Nsel months
    Alldates   <- Manager %>% select(date) %>% unique() %>% arrange(date)
    Roll_Dates <- Alldates %>% pull(date) %>% fmxdat::Date_Roller(., N = Nsel)

    # Join the benchmark to the full history once
    # Joined by month label as fund and index month-end dates differ
    # Named dfreg_full so it is not overwritten by the window data below
    dfreg_full <- left_join(
        Manager,
        BM_Monthly %>% select(YM, BM_Monthly) %>% unique(),
        by = "YM"
    )

    # Regression for one rolling window
    Map_roller_regression <- function(dfreg_full, Roll_Dates) {

        # Uncomment to step through one window
        # Dsel <- Roll_Dates[[41]]

        Dsel <- Roll_Dates
        dfreg <- dfreg_full %>% filter(date %in% Dsel)

        # Skip windows with missing months
        if(nrow(dfreg) != Nsel) {
            warning(glue::glue("Row mismatch for {unique(dfreg_full$Name)} at {Dsel[1]}"))
            return(NULL)
        }

        # Fund returns regressed on benchmark returns (intercept is alpha, slope is beta)
        lmreg <- lm(Returns ~ BM_Monthly, data = dfreg)

        # Tidy the output into one row per window with Alpha and Beta columns
        Output <- lmreg %>% broom::tidy() %>%
            select(term, coef = estimate) %>%
            mutate(term = ifelse(grepl("Intercept", term), "Alpha",
                                 ifelse(grepl("BM_M", term), "Beta", NA))) %>%
            # Each estimate is dated at the end of its window
            mutate(Manager = unique(dfreg$Name), date = last(Dsel)) %>%
            spread(term, coef)

        Output
    }

    # Run the regression over every window for this fund
    result_pm <- Roll_Dates %>% map_df(~Map_roller_regression(dfreg_full, .))

    result_pm
}