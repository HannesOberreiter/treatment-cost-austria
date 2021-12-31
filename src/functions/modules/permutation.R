# Permtuation Test Helpers
fPermTest <- function(df, col, stat = "diff in medians") {
    form <- as.formula(glue::glue("costs ~ {col}"))
    df %>%
        group_by(year_long) %>%
        nest() %>%
        mutate(
            point_estimate = map(data, fPermPoint, form = form, stat = stat),
            point_ci = map(data, fPermCI, form = form, stat = stat, point = point_estimate),
            null_distr = map(data, fPermNullDistr, form = form, stat = stat),
            p_value = map2(point_estimate, null_distr, fPermPvalue)
        )
}
fPermPoint <- function(df, form, stat) {
    data <- df %>%
        infer::specify(form)
    data %>%
        infer::calculate(stat)
}
fPermCI <- function(df, form, stat, point) {
    data <- df %>%
        infer::specify(form)
    boot <- data %>%
        infer::generate(reps = 1000, type = "bootstrap") %>%
        infer::calculate(stat = stat)
    boot %>%
        infer::get_confidence_interval(point_estimate = point[[1]], level = .95, type = "se")
}
fPermNullDistr <- function(df, form, stat) {
    df %>%
        infer::specify(form) %>%
        infer::hypothesize(null = "independence") %>%
        infer::generate(reps = 5000, type = "permute") %>%
        infer::calculate(stat)
}
fPermPvalue <- function(point, distribution) {
    infer::get_p_value(distribution, point, direction = "two-sided")
}

fPermMedianLabel <- function(point_estimate, point_ci) {
    latex2exp::TeX(
        sprintf(
            "$\\Delta | \\tilde{x} |$ = $%s$ (95%s CI: $%.1f$ - $%.1f$)",
            format(round(point_estimate$stat, 1), nsmall = 1),
            "%",
            point_ci$lower_ci,
            point_ci$upper_ci
        )
    )
}

fPermFacetLabel <- function(stat) {
    # Statistical Information for stat plot
    point_estimate <- stat %>%
        pull(point_estimate) %>%
        unlist()
    p_value <- stat %>%
        pull(p_value) %>%
        unlist() %>%
        round(., 2) %>%
        format(., nsmall = 2) %>%
        stringr::str_replace(., "0.00", "<0.01")
    point_ci <- stat %>%
        pull(point_ci) %>%
        unlist()
    lower_ci <- point_ci[names(point_ci) == "lower_ci"]
    upper_ci <- point_ci[names(point_ci) == "upper_ci"]

    latex2exp::TeX(
        sprintf(
            "$\\textit{p}$ = $%s$, $\\Delta | \\tilde{x} |$ = $%s$ (95%s CI: $%.1f$ - $%.1f$)",
            p_value,
            format(round(point_estimate, 1), nsmall = 1),
            "%",
            lower_ci,
            upper_ci
        )
    )
}

fPlotPermutation <- function(year_long, data, point_estimate, point_ci, null_distr, p_value) {
    # Plotting Permutation Test of Medians
    # Takes output from permutation functions
    # Returns ggplot
    lab <- latex2exp::TeX(
        sprintf(
            "Year: %s, $\\textit{p}$ = $%s$",
            year_long,
            as.character(ifelse(p_value == 0, "***", p_value))
        )
    )
    lab2 <- fPermMedianLabel(point_estimate, point_ci)

    null_distr %>%
        infer::visualise() +
        infer::shade_confidence_interval(point_ci, fill = colorBlindBlack8[5], color = colorBlindBlack8[2]) +
        infer::shade_p_value(obs_stat = point_estimate, direction = "two-sided", color = colorBlindBlack8[7]) +
        ggplot2::scale_y_continuous(breaks = scales::breaks_pretty()) +
        ggplot2::ylab("Count [#]") +
        ggplot2::xlab("Median Difference [EUR]") +
        ggplot2::labs(
            title = lab,
            subtitle = lab2
        ) +
        ggplot2::theme(
            panel.grid.major.y = element_line(),
            plot.title = element_text(size = 10),
            plot.subtitle = element_text(size = 10)
        )
}