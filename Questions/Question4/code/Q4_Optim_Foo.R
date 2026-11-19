Q4_Optim_Foo <- function(Type = "minvol", Sigma, ref, printmsg = TRUE){

    Safe_Optim <- purrr::safely(quadprog::solve.QP)

    # limits for the assets in this covariance matrix
    Cons <- Q4_Constraints(colnames(Sigma), ref)

    # minimum variance (no return vector)
    Opt_W <-
        Safe_Optim(Dmat = Sigma, dvec = rep(0, ncol(Sigma)),
                   Amat = Cons$Amat, bvec = Cons$bvec, meq = 1)

    if( is.null(Opt_W$error)){

        optimw <-
            tibble(Tickers = colnames(Sigma), weights = Opt_W$result$solution) %>%
            rename(!!Type := weights)

        if(printmsg)   optimw <- optimw %>% mutate(Result = glue::glue("Converged: {Type}"))

    } else {

        # equal weights if the solver fails
        optimw <- tibble(Tickers = colnames(Sigma), weights = 1/ncol(Sigma)) %>%
            rename(!!Type := weights)

        if(printmsg)   optimw <- optimw %>% mutate(Result = glue::glue("Failed to Converge: {Type}"))

    }
    optimw
}