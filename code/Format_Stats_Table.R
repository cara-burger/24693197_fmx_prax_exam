Format_Stats_Table <- function(Tab_stats, BM){

    library(gt)

    Make_perc <-
        c( "Cum Returns", "Returns (Ann.)", "Returns Excess (Ann.)", "SD (Ann.)",
           "Avg DD", "Tracking Error")

    Rows_to_Perc <-
        Tab_stats %>% mutate(RN = row_number()) %>% filter(Info %in% Make_perc) %>% pull(RN)

    colnams <- colnames(Tab_stats)[-1:-2]

    Cols_length <- ncol(Tab_stats)

    Tab_stats %>%

        gt::gt(groupname_col = 'Period', caption = 'Fund Moments Comparison') %>%
        tab_header(title = glue::glue("Index Statistics: Relative to {BM}")) %>%
        fmt_percent(
            columns = 3:Cols_length,
            rows = Rows_to_Perc,
            decimals = 1
        ) %>%

        sub_missing(
            columns = all_of(colnams),
            missing_text = '-'
        ) %>%

        tab_style(
            style = list(
                cell_fill(color = 'gray27', alpha = 0.15),
                cell_text(size = 'large', weight = 'bold', align = 'left')
            ),
            locations = cells_row_groups())

}
