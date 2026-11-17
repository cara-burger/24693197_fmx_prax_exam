Q1_Garch_Sigma <- function(df){

    # Returns as xts
    porteqw <- df %>% arrange(date) %>% tbl_xts(cols_to_xts = Returns)

    # GJR-GARCH(1,1), AR(1) mean model, student t distribution
    gjrgarch11 = ugarchspec(variance.model = list(model = c("sGARCH","gjrGARCH","eGARCH","fGARCH","apARCH")[2],
                                                  garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                            distribution.model = c("norm", "snorm", "std", "sstd", "ged", "sged", "nig", "ghyp", "jsu")[3])

    garchfit2 = ugarchfit(spec = gjrgarch11, data = porteqw)

    # Conditional volatility (sigma) as a tibble
    sigma <- sigma(garchfit2) %>% xts_tbl()
    colnames(sigma) <- c("date", "sigma")
    sigma <- sigma %>% mutate(date = as.Date(date))

    # Annualise sigma to match the rolling SD scale, and add the ticker name
    sigma <- sigma %>% mutate(sigma = sigma * sqrt(252), Tickers = unique(df$Tickers))

    list(
        Sigma = sigma,
        # Parameter estimates and p-values
        Coefs = garchfit2@fit$matcoef,
        # Persistence (for GJR-GARCH this includes the leverage term)
        Persistence = tibble(Tickers = unique(df$Tickers), Persistence = persistence(garchfit2))
    )

}