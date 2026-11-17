Q1_Level_to_Returns <- function(df){

    df %>%
        # Sort by date so that lag() always refers to the previous trading day
        arrange(date) %>%
        # Calculate returns within each index (never across two indexes)
        group_by(Ticker) %>%
        # Simple return = today's level / yesterday's level - 1
        mutate(Returns = TRI / lag(TRI) - 1) %>%
        # First date of each idex has no previous level (drop NA)
        filter(!is.na(Returns)) %>%
        ungroup() %>%
        # Rename Ticker to Tickers so it matches the local index return data
        select(date, Tickers = Ticker, Name, Returns)

}