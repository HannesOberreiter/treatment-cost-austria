# Labels for Plots
fCoinLabel <- function(kruskal, eff) {
    df <- kruskal %>% map_int(~ .x@statistic@df)
    stat <- kruskal %>% map_dbl(~ statistic(.x))
    p <- kruskal %>% map_chr(~ format.pval(pvalue(.x), eps = 0.001))
    TeX(
        sprintf(
            "$\\chi^2$(%i) = $%.2f$, $\\textit{p}$ = $%s$, $\\eta^2(H)$ = $%.2f$",
            df,
            stat,
            p,
            abs(unlist(eff))
        )
    )
}
# Saving Plots
fSaveImages <- function(currentplot, filename, path = "output/figs/", w = 7.5, h = 4, ...) {
    ggplot2::ggsave(paste0(path, filename, ".pdf"), currentplot, width = w, height = h, ...)
    ggplot2::ggsave(paste0(path, filename, ".png"), currentplot, width = w, height = h, dpi = 320, ...)
    invisible(currentplot)
}