# Helper function to call statistics and return friendly list
fKruskal <- function(df, sub, col) {
    l <- list()
    l$kruskal <- df %>%
        split(.$year_long) %>%
        map(~ fCoinKruskal(.x$costs, .x[[col]]))
    l$effect <- l$kruskal %>%
        purrr::map_dbl(~ fEffektSize(.x))
    # l$kruskal_treatment <- sub %>%
    #    split(.$year) %>%
    #    map(~ fCoinKruskal(.x$costs, as.factor(.x[[col]])))
    # l$effect_treatment <- l$Kruskal_Treatment %>%
    #    map(~ fEffektSize(.x))
    return(l)
}


# Kruskall ####
# COIN Permutation Test
# kruskal_wallis without coin gives the same test results
# effekt size is based on coin kruskal wallis, if we use bootstrap we can get
# CI, effektsize is multiplied by 100 the % explained by the variable of the variance
fCoinKruskal <- function(depentend, indepentend) {
    x <- dplyr::tibble(dep = depentend, ind = indepentend)
    coin::kruskal_test(
        dep ~ 0 + ind,
        data = x,
        distribution = approximate(
            nresample = 10000
        )
    )
}
# Effect Size ####
# The interpretation values commonly in published literature are:
# 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).
# fEffektSize <- function(depentend, indepentend){
#   rstatix Variant
#   x <- tibble(dep = depentend, ind = indepentend)
#   kruskal_effsize(
#     dep ~ 0 + ind, data = x,
#     ci = T, ci.type = "basic"
#   )
# }
fEffektSize <- function(krus) {
    # simpler Calculation of Effect size no CI needed would also be
    # caught by multiple testing
    # Formula
    # \eta^2_H = \frac{H-k+1}{n-k}
    H <- coin::statistic(krus)
    n <- nrow(krus@statistic@x)
    k <- krus@statistic@df + 1
    effect <- (H - k + 1) / (n - k)
    return(effect)
}