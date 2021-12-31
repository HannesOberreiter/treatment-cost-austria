include_custom <- function(p) {
    if (knitr::is_latex_output()) {
        stringr::str_replace(p, ".png", ".pdf")
    }
    knitr::include_graphics(p)
}

ft <- function(x, ...) {
    format(x, big.mark = ",", ...)
}
