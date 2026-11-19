Q4_Roll_Optimizer <- function(return_mat, EOM_datevec, ref, LookBackSel = 120){

    # returns over the look-back period up to the rebalance date
    return_df_used <- return_mat %>% filter(date >= EOM_datevec %m-% months(LookBackSel), date <= EOM_datevec)

    # skip the date if the look-back period is not full
    if(return_df_used %>% nrow() < LookBackSel) return(NULL)

    # keep assets with at least 5 years of returns
    return_df_used <- return_df_used[, colSums(!is.na(return_df_used)) >= 60]

    # fill gaps with draws from each asset's own distribution
    return_df_used <- Q4_Impute_Missing_Returns(return_df_used, impute_returns_method = "Drawn_Distribution_Own")

    return_mat_Nodate <- data.matrix(return_df_used[, -1])

    # Ledoit-Wolf covariance for the look-back period
    Sigma <- RiskPortfolios::covEstimation(return_mat_Nodate, control = list(type = "lw"))
    diag(Sigma) <- diag(Sigma) + 1e-8

    My_Weights <-
        Q4_Optim_Foo(Type = "minvol", Sigma, ref, printmsg = TRUE) %>%
        mutate(date = EOM_datevec, Look_Back_Period = LookBackSel)

    My_Weights
}