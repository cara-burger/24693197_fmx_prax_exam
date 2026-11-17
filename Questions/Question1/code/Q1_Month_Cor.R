Q1_Month_Cor <- function(data){

    data_byhand <- data %>% select(date, Tickers, Return) %>% spread(Tickers, Return)

    # Set missing returns to zero
    data_byhand[is.na(data_byhand)] <- 0

    cormat <- cor( data_byhand %>% select(-date))

    # Remove each stock's correlation with itself before averaging
    diag(cormat) <- NA

    tibble(date = max(data$date), Avg_Cor = mean(cormat, na.rm = T))
}