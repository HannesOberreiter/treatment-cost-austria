fQQPlot <- function(df) {
    df %>%
        filter(costs < 100) %>%
        ggplot(aes(sample = costs, group = year, color = year)) +
        geom_qq(alpha = 0.5) +
        geom_qq_line() +
        theme_classic() +
        ylab("Observed (Expenses / Colony) Quantile Distribution") +
        xlab("Theoretical Quantile Distribution") +
        scale_color_manual(values = colorBlindBlack8[c(2, 4, 6)], name = "Survey")
}