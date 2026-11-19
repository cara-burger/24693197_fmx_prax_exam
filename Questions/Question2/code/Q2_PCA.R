# Share of variation in constituent returns explained by the first principal component,
# for one index in one year
Q2_PCA <- function(Cons, Stocks){

    Year_Sel <- format(unique(Cons$date), "%Y")

    # Log returns of the constituents during the year
    data <-
        Stocks %>%
        filter(Tickers %in% Cons$Tickers, format(date, "%Y") == Year_Sel) %>%
        filter(!is.na(Returns)) %>%
        mutate(Returns = log(1 + Returns))

    data <-
        data %>% mutate(Returns = ifelse(Returns > 0.25, 0.25,
                                         ifelse(Returns < -0.25, -0.25, Returns)))

    data <-
        data %>%
        group_by(Tickers) %>%
        mutate(Returns = Returns - mean(Returns)) %>%
        ungroup()

    data_byhand <-
        data %>% select(date, Tickers, Returns) %>% spread(Tickers, Returns)

    cormat <-
        cor( data_byhand %>% select(-date), use = "pairwise.complete.obs")

    eval <- eigen(cormat, symmetric=TRUE)$values

    prop = eval / sum(eval)

    tibble(Fund = unique(Cons$Fund), Year = Year_Sel, Prop = prop[1], N_Stocks = ncol(cormat))
}