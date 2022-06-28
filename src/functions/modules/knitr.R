include_custom <- function(p) {
    if (knitr::is_latex_output()) {
        stringr::str_replace(p, ".png", ".pdf")
    }
    knitr::include_graphics(p)
}

ft <- function(x, ...) {
    base::format(x, big.mark = ",", ...)
}

fr <- function(x, r = 1, ...) {
    base::round(x, r) |>
        base::format(big.mark = ",", nsmall = r, ...)
}
