# First Chapter Survey Stats
r_survey <- list()

# Reponse Table --------------------------------------------------------------------
r_survey$response <- statsAut %>%
    add_column(
        survey_beekeeper = dfClean %>% count(year) %>% pull(n),
        survey_colonies = dfClean %>% group_by(year) %>% summarise(sh = sum(hives_winter)) %>% pull(sh)
    ) %>%
    mutate(
        rate_beekeeper = round(survey_beekeeper / beekeeper * 100, 1),
        rate_colonies = round(survey_colonies / colonies * 100, 1)
    )

# Distribution Map --------------------------------------------------------------------
source("src/misc/map.R")

# Summary --------------------------------------------------------------------
r_survey$summary <- list()

r_survey$summary$survey_table <- dfClean %>%
    group_by(year) %>%
    summarise(
        min = format(round(min(costs), 2), nsmall = 2),
        mean = format(round(mean(costs), 2), nsmall = 2),
        gmean = format(round(fGeoMean(costs), 2), nsmall = 2),
        median = format(round(median(costs), 2), nsmall = 2),
        max = format(round(max(costs), 2), nsmall = 2),
        n = n()
    ) %>%
    add_column(type = "Survey", .after = "year")

r_survey$summary$survey_total <- tibble(
    year = "total",
    min = format(round(min(dfClean$costs), 2), nsmall = 2),
    mean = format(round(mean(dfClean$costs), 2), nsmall = 2),
    gmean = format(round(fGeoMean(dfClean$costs), 2), nsmall = 2),
    median = format(round(median(dfClean$costs), 2), nsmall = 2),
    max = format(round(max(dfClean$costs), 2), nsmall = 2),
    n = nrow(dfClean)
)

r_survey$summary$estimate_table <- dfClean %>%
    group_by(year) %>%
    summarise(
        min = format(round(min(t_estimated), 2), nsmall = 2),
        mean = format(round(mean(t_estimated), 2), nsmall = 2),
        gmean = format(round(fGeoMean(t_estimated), 2), nsmall = 2),
        median = format(round(median(t_estimated), 2), nsmall = 2),
        max = format(round(max(t_estimated), 2), nsmall = 2),
        n = n(),
    ) %>%
    add_column(type = "Estimated", .after = "year")


# Summary Plot --------------------------------------------------------------------
# text1 <- rep(paste0(
#    "Max.:", "\n",
#    "Min.:", "\n",
#    "Mean:", "\n",
#    "Median:", "\n"
# ), 3)
# text2 <- paste0(
#    r_survey$summary$survey_table$max, "\n",
#    r_survey$summary$survey_table$min, "\n",
#    r_survey$summary$survey_table$mean, "\n",
#    r_survey$summary$survey_table$median, "\n"
# )
## Based on Cedric Scherer raincloud chart
## https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/#section2
p <- ggplot(dfClean, aes(x = year, y = costs, fill = year)) +
    ggdist::stat_halfeye(
        adjust = .5,
        width = .6,
        .width = 0,
        justification = -.2,
        point_colour = NA
    ) +
    geom_boxplot(
        width = .15,
        outlier.shape = NA,
        alpha = 0.5
    ) +
    gghalves::geom_half_point(
        side = "l",
        size = 0.5,
        alpha = 0.1,
        transformation = position_jitter(width = 0.05, height = 0.1)
    ) +
    scale_color_manual(
        values = colorBlindBlack8[c(2, 4, 6)],
        aesthetics = "fill", name = "Survey"
    ) +
    xlab("") +
    ylab("Expenses / Colony [EUR]") +
    # Our three extension arrows
    geom_segment(
        x = 1, y = 45,
        xend = 1, yend = 52,
        lineend = "round",
        color = "red",
        linejoin = "round",
        size = 0.5,
        arrow = arrow(length = unit(0.1, "inches"))
    ) +
    geom_segment(
        x = 2, y = 45,
        xend = 2, yend = 52,
        lineend = "round",
        linejoin = "round",
        color = "red",
        size = 0.5,
        arrow = arrow(length = unit(0.1, "inches"))
    ) +
    geom_segment(
        x = 3, y = 45,
        xend = 3, yend = 52,
        lineend = "round",
        linejoin = "round",
        color = "red",
        size = 0.5,
        arrow = arrow(length = unit(0.1, "inches"))
    ) +
    #    annotate(
    #        "text",
    #        label = text1,
    #        x = c(1.20, 2.20, 3.20), y = c(45, 45, 45),
    #        hjust = "left"
    #    ) +
    #    annotate(
    #        "text",
    #        label = text2,
    #        x = c(1.60, 2.60, 3.60), y = c(45, 45, 45),
    #        hjust = "right"
    #    ) +
    scale_y_continuous(
        limits = c(0, 50),
        breaks = c(seq(0, 50, 5))
    ) +
    ggplot2::scale_x_discrete(
        labels = function(x) {
            paste0("20", x)
        }
    ) +
    coord_cartesian(xlim = c(1.2, NA)) +
    ggplot2::theme(
        legend.position = "none",
        panel.grid.major.y = element_line()
    )

fSaveImages(p, "distr-year", w = 8.5, h = 5)
rm(text1, text2, p)