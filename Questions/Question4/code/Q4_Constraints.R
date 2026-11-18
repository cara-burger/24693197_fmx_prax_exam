Q4_Constraints <- function(Assets, ref, LB = 0, UB = 0.35){

    # group limit for each asset (NA if the asset is not in that group)
    Group_Caps <-
        ref %>%
        mutate(Equity = ifelse(Class == "Equity", 0.75, NA),
               Bonds = ifelse(Class == "Bonds", 0.25, NA),
               Commodities = ifelse(Class == "Commodities", 0.15, NA),
               Global = ifelse(Region == "Global", 0.60, NA)) %>%
        select(Name, Equity, Bonds, Commodities, Global)

    # 1 if the asset is in the group, in the same order as the covariance matrix
    avec_gt <-
        Group_Caps %>%
        filter(Name %in% Assets) %>%
        mutate(across(-Name, ~ifelse(is.na(.x), 0, 1))) %>%
        arrange(match(Name, Assets)) %>% select(-Name)

    # limit of each group
    bvec_gt <- Group_Caps %>% select(-Name) %>% apply(., 2, max, na.rm = T)

    # upper limits are multiplied by minus one
    groups_mat <- avec_gt %>% mutate(across(everything(), ~ . * -1)) %>% as.matrix()

    n <- length(Assets)

    Amat <- cbind(rep(1, n), -diag(n), diag(n), groups_mat)
    bvec <- c(1, -rep(UB, n), rep(LB, n), -bvec_gt)

    list(Amat = Amat, bvec = bvec)
}