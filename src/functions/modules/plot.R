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