fPairwiseMM <- function(d) {
    x <- tibble(
        xmin = character(),
        xmax = character(),
        mean_diff = numeric(),
        median_diff = numeric()
    )
    for (i in 1:(nrow(d) - 1)) {
        xmin <- d[i, 1]
        meani <- d[i, 3]
        mediani <- d[i, 4]
        for (j in (i + 1):nrow(d)) {
            xmax <- d[j, 1]
            meanj <- d[j, 3]
            medianj <- d[j, 4]

            dummy <- cbind(
                xmin = xmin, xmax = xmax,
                mean_diff = round(abs(meani - meanj), 1),
                median_diff = round(abs(mediani - medianj))
            )
            names(dummy) <- c("xmin", "xmax", "mean_diff", "median_diff")
            x <- x %>% dplyr::add_row(dummy)
        }
    }
    x <- x %>% add_column(
        year_long = d$year_long[1],
        tex = TeX(
            sprintf(
                "$\\Delta | \\tilde{x} |$ = $%.1f$",
                x$median_diff
            )
        )
    )
    return(x)
}