Dispersion_Prep <- function(df_used, BMUse, BMSel = "J200", NSel = 60, CapDisp = 0.45){

    # Cap extreme returns at +/- 50%
    df_use <-
        df_used %>% select(date, Tickers, Return, Wt = all_of(BMSel)) %>% filter(date > ymd(20050101)) %>% filter(!is.na(Wt)) %>%
        mutate(Return = ifelse(Return > 0.5, 0.5,
                               ifelse(Return < -0.5, -0.5, Return))) %>% filter(!is.na(Return))

    # Rolling returns and weights of each stock, and the rolling benchmark return
    Roll_M_disp <-
        df_use %>% group_by(Tickers) %>% mutate(roll_Rt = RcppRoll::roll_prod(1+Return, n = NSel, fill = NA, align = "right")-1,
                                                roll_wt = RcppRoll::roll_mean(Wt, n = NSel, fill = NA, align = "right")) %>%
        left_join(., BMUse %>% mutate(roll_BM = RcppRoll::roll_prod(1+BM, n = NSel, fill = NA, align = "right")-1) %>% select(date, roll_BM) %>% unique, by = "date") %>%
        filter(!is.na(roll_wt)) %>%
        filter(!is.na(roll_BM)) %>%
        # Dispersion across stocks on each date
        group_by(date) %>%
        mutate(Idx_U = sum(roll_Rt / 1/n(), na.rm=T)) %>%
        summarise(Dispersion_U = sqrt( sum ( (1/n())*(roll_Rt - Idx_U)^2, na.rm = TRUE) ),
                  Dispersion_W = sqrt( sum ( (roll_wt)*(roll_Rt - roll_BM)^2, na.rm = TRUE) ))

    # Cap very large dispersion values
    Roll_M_disp %>% mutate(type = BMSel, N = NSel) %>% mutate(across(starts_with("Disp"), ~ifelse(. > CapDisp, CapDisp, .)))
}
