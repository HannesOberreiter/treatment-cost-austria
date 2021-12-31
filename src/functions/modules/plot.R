# Labels for Plots
fCoinLabel <- function(statistic) {
    kruskal <- statistic$kruskal
    eff <- statistic$effect %>%
        round(., 2) %>%
        format(., nsmall = 2) %>%
        stringr::str_replace(., "0.00", "<0.01")
    df <- kruskal %>% map_int(~ .x@statistic@df)
    stat <- kruskal %>% map_dbl(~ statistic(.x))
    p <- kruskal %>% map_chr(~ format.pval(pvalue(.x), eps = 0.001))
    TeX(
        sprintf(
            "$\\chi^2$(%i) = $%.2f$, $\\textit{p}$ = $%s$, $\\eta^2(H)$ = $%s$",
            df,
            stat,
            p,
            eff
        )
    )
}

# Saving Plots
fSaveImages <- function(currentplot, filename, path = "output/figs/", w = 7.5, h = 4, ...) {
    ggplot2::ggsave(paste0(path, filename, ".pdf"), currentplot, width = w, height = h, encoding = "ISOLatin9.enc", ...)
    ggplot2::ggsave(paste0(path, filename, ".png"), currentplot, width = w, height = h, dpi = 320, ...)
    invisible(currentplot)
}

fPlotFactor <- function(df, col, statistic, sigY) {
    # Plotting of Boxplot for Kruskal Wallis Statistics
    # df main dataframe
    # col is column for groups
    # statistics test result from permutation kruskal wallis test
    # sigY is position of pairwise Statistics
    facetLabels <- tibble(
        year_long = df %>% pull(year_long) %>% sort() %>% unique(),
        label = fPermFacetLabel(statistic),
        # label = fCoinLabel(statistic)
    )


    difLabel <- df %>%
        group_by(.data[[col]], year_long) %>%
        summarize(mean = mean(costs), median = median(costs)) %>%
        ungroup() %>%
        group_split(year_long, remove = F) %>%
        map_dfr(~ fPairwiseMM(.x))

    countLabel <- df %>%
        dplyr::count(year_long, .data[[col]])

    df %>%
        filter(costs <= 70) %>%
        ggplot(
            aes(
                y = costs,
                x = .data[[col]],
                color = year_long
            )
        ) +
        ggforce::geom_sina(
            show.legend = FALSE,
            color = colorBlindBlack8[1],
            alpha = 0.1,
        ) +
        geom_boxplot(outlier.alpha = 0, show.legend = F, alpha = 0) +
        stat_summary(
            fun = mean, geom = "point", show.legend = F, color = colorBlindBlack8[[8]], shape = 18, size = 4
        ) +
        geom_text(
            data = countLabel,
            mapping = aes(x = .data[[col]], label = paste0("n = ", n), y = 0),
            size = 2.5,
            vjust = 1.5,
            color = colorBlindBlack8[[1]]
        ) +
        theme_classic() +
        theme(
            axis.text.x = element_text(size = 12)
        ) +
        ggsignif::geom_signif(
            data = difLabel,
            aes(
                xmin = xmin, xmax = xmax,
                annotations = paste(tex),
                y_position = sigY
            ),
            textsize = 3, color = "black", manual = TRUE, parse = TRUE
        ) +
        scale_y_continuous(
            limits = c(0, 70),
            breaks = c(seq(-15, 100, 5))
        ) +
        scale_color_manual(
            values = colorBlindBlack8[c(2, 4, 6)], name = "Survey"
        ) +
        geom_text(
            data = facetLabels,
            aes(label = label, x = 1.5),
            parse = TRUE,
            y = 70,
            inherit.aes = FALSE,
            size = 3
        ) +
        ylab("Expenses/Colony [EUR]") +
        facet_wrap(
            ~year_long,
            labeller = labeller(
                year = function(x) {
                    return(paste("Survey", x))
                }
            )
        )
}