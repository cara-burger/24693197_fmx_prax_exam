Q3_Beta_Boxplot <- function(Rolling_Beta, Months_back = 60, Fund = "ABC_Fund") {

    # Average beta per fund over the lookback period
    # safe_month_min avoids dropping a month when month-end days differ
    # Only funds with a full history over the period are kept
    avg_beta <-
        Rolling_Beta %>%
        filter(date >= fmxdat::safe_month_min(last(date), N = Months_back)) %>%
        group_by(Manager) %>%
        filter(n() >= Months_back) %>%
        summarise(Beta = mean(Beta, na.rm = TRUE))

    gsav <-
        avg_beta %>% filter(Manager != Fund) %>%
        ggplot() +
        # Boxplot shows the typical range of peer betas, jitter shows each peer
        geom_boxplot(aes("Beta", Beta), alpha = 0.5, fill = '#0f70ee') +
        geom_jitter(aes("Beta", Beta), size = 3) +
        # ABC Fund shown on top of its peers
        geom_point(data = avg_beta %>% filter(Manager == Fund), aes("Beta", Beta), colour = "red", size = 6) +
        geom_label(data = avg_beta %>% filter(Manager == Fund), aes("Beta", Beta, label = "ABC Fund"), nudge_x = 0.12) +
        # Line where a fund moves one for one with the market
        geom_hline(yintercept = 1) +
        coord_cartesian(ylim = c(0, max(avg_beta$Beta))) +
        labs(title = "Most active managers closely follow the market",
            subtitle = "Each dot is one active manager. The higher the dot, the more its returns simply follow the market.",
            caption = "Black line: moves one for one with the market (Capped SWIX). Blue box: where the middle half of managers sit.",
            x = "", y = "Market sensitivity (beta)") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1),
            breaks = scales::pretty_breaks(10)) +
        fmxdat::theme_fmx() +
        theme(axis.text.x = element_blank())

    gsav
}