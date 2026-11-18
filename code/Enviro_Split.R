# Split months into high, low and other environments using quantile cut-offs of the column Value
Enviro_Split <- function(df, Hi = 0.85, Lo = 0.25, Label = ""){

    if( !"Value" %in% names(df)) stop("... for Enviro_Split to work, provide the column to split on called 'Value'")

    df %>%
        mutate( hiQ = quantile(Value, Hi),
                loQ = quantile(Value, Lo)) %>%
        mutate(Enviro = ifelse(Value > hiQ, paste("High", Label),
                               ifelse(Value < loQ, paste("Low", Label),
                                      "Other")))
}