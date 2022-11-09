# Chapter overview of treatment methods
r_treatment <- list()

# Heatmap ----
r_treatment$heat_data <- dfClean %>%
    dplyr::select(T_drone_01:T_other_12) %>%
    # dplyr::select(T_drone_01:T_other_12) %>%
    summarise(across(everything(), sum, na.rm = TRUE)) %>%
    pivot_longer(everything()) %>%
    separate(col = name, into = c("treatment", "month"), sep = -2, convert = TRUE) %>%
    mutate(
        treatment = stringr::str_replace(treatment, "T_synthetic_.*", "T_synthetic_")
    ) %>%
    group_by(treatment, month) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(
        text_color = ifelse(value > 50, "white", "black"),
        season = case_when(
            between(month, 1, 2) ~ "Spring",
            between(month, 3, 7) ~ "Summer",
            between(month, 8, 12) ~ "Winter",
            TRUE ~ "-"
        ),
        season = forcats::fct_relevel(season, "-", after = Inf)
    ) %>%
    left_join(treatmentList, by = c("treatment" = "tsingle"))

r_treatment$seasons <- r_treatment$heat_data %>%
    group_by(season) %>%
    summarise(
        x = mean(month),
        min = min(month),
        max = max(month),
        y = 0
    )
r_treatment$months <- c(
    "Apr", "May", "Jun",
    "Jul", "Aug", "Sep",
    "Oct", "Nov", "Dec",
    "Jan", "Feb", "Mar"
)
p <- r_treatment$heat_data %>%
    ggplot(aes(y = tname, x = month)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(color = text_color, label = value)) +
    geom_text(
        data = r_treatment$seasons,
        aes(x = x, y = y, label = season),
        inherit.aes = FALSE,
        color = "grey30",
        nudge_y = 0.2
    ) +
    geom_rect(
        data = r_treatment$seasons,
        aes(xmin = min, xmax = max, ymin = y - 0.1, ymax = y - 0.11),
        color = "grey30",
        inherit.aes = FALSE
    ) +
    scale_fill_viridis_c(
        breaks = trans_breaks("log", function(x) 10^x, n = 10),
        trans = "log",
        option = "inferno",
        direction = -1,
        na.value = "white"
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(
        breaks = 1:12,
        labels = r_treatment$months,
        sec.axis = dup_axis(name = "Months")
    ) +
    ggplot2::coord_equal(expand = FALSE, clip = "off") +
    xlab("Seasons") +
    ylab("Treatment Method") +
    # ggplot2::facet_grid(~season, scales = "free_x") +
    labs(fill = "Answers") +
    ggplot2::theme(
        axis.ticks = element_blank(),
        axis.line = element_blank()
    )
fSaveImages(p, "treatment-heat", w = 10, h = 8.5)

# Top Treatment Methods -----
calcTop <- function(df, filter) {
    df %>%
        add_count(year, c_short_od) %>%
        add_count(c_short_od) %>%
        group_by(year) %>%
        mutate(
            year_n = n(),
            year_loss = (sum(hives_lost_e) / sum(hives_winter)) * 100,
            year_loss_ci = list(fLossCI(hives_lost_e, hives_spring_e)),
            # year_costs = mean(log10(costs))
            # year_costs = fGeoMean(costs),
            # year_costs = median(costs),
            year_costs = mean(costs),
        ) %>%
        filter(nn >= filter) %>%
        ungroup() %>%
        group_by(c_short_od) %>%
        mutate(

            # observed_costs = fGeoMean(costs),
            # observed_costs_sd = fGeoSD(costs),
        ) %>%
        group_by(year, c_short_od, year_n, year_loss, year_costs, year_loss_ci) %>%
        summarise(
            n = n(),
            observed_loss = (sum(hives_lost_e) / sum(hives_winter)) * 100,
            observed_loss_ci = list(fLossCI(hives_lost_e, hives_spring_e)),
            observed_costs = mean(costs),
            observed_costs_sd = sd(costs),
            observed_costs_se = fSE(costs),
            observed_costs_median = median(costs),
            # observed_costs = mean(log10(costs)),
            # observed_costs = fGeoMean(costs),
            # observed_costs_iqr = fGeoSD(costs),
            observed_costs = first(observed_costs),
            observed_costs_sd = first(observed_costs_sd),
            efficient = ifelse(observed_loss > first(year_loss), "high-loss", "low-loss"),
            # economical = ifelse(observed_costs > first(year_costs), "high-expense", "low-expense")
            economical = ifelse(observed_costs > first(year_costs), "high-expense", "low-expense")
        ) %>%
        ungroup() %>%
        group_by(year) %>%
        mutate(
            loss_quantile = list(quantile(observed_loss, probs = c(0.33, 0.67))),
            cost_quantile = list(quantile(observed_costs, probs = c(0.33, 0.67)))
        ) %>%
        ungroup() %>%
        group_by(year, c_short_od) %>%
        mutate(
            quantile_efficient = case_when(
                observed_loss < first(loss_quantile)[1] ~ "low-loss",
                observed_loss > first(loss_quantile)[2] ~ "high-loss",
                TRUE ~ "intermediate-loss"
            ),
            quantile_economical = case_when(
                observed_costs < first(cost_quantile)[1] ~ "low-expense",
                observed_costs > first(cost_quantile)[2] ~ "high-expense",
                TRUE ~ "intermediate-expense"
            )
        ) %>%
        ungroup() %>%
        unnest_wider(observed_loss_ci)
}

calcQuantile <- function(df) {
    df %>%
        group_by(year) %>%
        summarise(
            year = paste0("20", first(year)),
            loss_low = first(loss_quantile)[1],
            loss_high = first(loss_quantile)[2],
            cost_low = first(cost_quantile)[1],
            cost_high = first(cost_quantile)[2]
        )
}

plotEE <- function(df, quantile) {
    p <- df %>%
        left_join(r_treatment$letter, by = c("c_short_od")) %>%
        mutate(
            # label_color = paste(efficient, economical, sep = " & ") %>% stringr::str_to_title(),
            label_color = paste(quantile_efficient, quantile_economical, sep = " & ") %>% stringr::str_to_title(),
            color = namedColors[label_color],
            year = paste0("20", year)
        ) %>%
        ggplot(aes(y = observed_costs, x = observed_loss, color = label_color)) +
        # geom_vline(aes(xintercept = year_loss), color = "#0072B2", linetype = "dashed") +
        # geom_hline(aes(yintercept = year_costs), color = "#0072B2", linetype = "dashed") +
        geom_vline(data = quantile, aes(xintercept = loss_low), color = "#0072B2", linetype = "dashed") +
        geom_vline(data = quantile, aes(xintercept = loss_high), color = "#0072B2", linetype = "dashed") +
        geom_hline(data = quantile, aes(yintercept = cost_low), color = "#0072B2", linetype = "dashed") +
        geom_hline(data = quantile, aes(yintercept = cost_high), color = "#0072B2", linetype = "dashed") +
        geom_linerange(
            aes(ymin = observed_costs - observed_costs_se, ymax = observed_costs + observed_costs_se),
            # color = "gray",
            alpha = 0.15,
            show.legend = FALSE
        ) +
        geom_linerange(
            aes(xmin = loss_lower_ci, xmax = loss_upper_ci),
            # color = "gray",
            alpha = 0.15,
            show.legend = FALSE
        ) +
        geom_point(size = 0.5) +
        geom_text_repel(
            aes(label = letter),
            fontface = "bold",
            show.legend = FALSE
        ) +
        annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1, color = "black") +
        annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, size = 1, color = "black") +
        facet_wrap(~year, ncol = 1, scales = "fixed", strip.position = "right") +
        ggplot2::scale_x_continuous(
            breaks = scales::pretty_breaks(n = 10),
            limits = c(0, NA),
            labels = scales::label_number(suffix = "\u0025", accuracy = 1),
            sec.axis = dup_axis()
        ) +
        ggplot2::scale_y_continuous(
            breaks = scales::pretty_breaks(n = 8),
            limits = c(0, NA),
            labels = scales::label_dollar(prefix = "", suffix = "€")
        ) +
        ggplot2::scale_colour_manual(
            # values = c("#D55E00", "#CC79A7", "#E69F00", "#009E73")
            values = namedColors
        ) +
        ylab("Mean Expenses/Colony [EUR] ± SE") +
        xlab("Mean Observed Colony Winter Loss [%] ± CI (95%)") +
        labs(color = "") +
        ggplot2::theme(
            legend.position = "bottom",
            axis.line.x = element_blank(),
            axis.title.x.bottom = element_blank(),
            # panel.grid.major = element_line(color = "gray80", linetype = "dotted", size = 0.1)
        ) +
        guides(colour = guide_legend(nrow = 3, override.aes = list(size = 8)))

    return(p)
}

createTable <- function(df) {
    df %>%
        left_join(r_treatment$letter) %>%
        arrange(letter) %>%
        select(letter, c_short_od, year, n, observed_costs_median, observed_costs, observed_costs_sd, observed_loss, loss_lower_ci, loss_upper_ci, efficient, economical, quantile_efficient, quantile_economical) %>%
        mutate(
            efficient = stringr::str_replace(quantile_efficient, "intermediate-loss", ""),
            economical = stringr::str_replace(quantile_economical, "intermediate-expense", "")
        )
}

r_treatment$top <- dfClean %>%
    calcTop(filter = 30)
r_treatment$big <- dfClean %>%
    filter(hives_winter > 25) %>%
    calcTop(filter = 15)
r_treatment$small <- dfClean %>%
    filter(hives_winter <= 25) %>%
    calcTop(filter = 20)
r_treatment$quantile <- r_treatment$top %>%
    calcQuantile()
r_treatment$quantile_big <- r_treatment$big %>%
    calcQuantile()
r_treatment$quantile_small <- r_treatment$small %>%
    calcQuantile()

r_treatment$year_loss <- r_treatment$top %>%
    group_by(year, year_loss_ci) %>%
    summarise(
        year_loss = first(year_loss)
    ) %>%
    unnest_wider(year_loss_ci)


r_treatment$letter <- r_treatment$top %>%
    group_by(c_short_od) %>%
    summarise(
        sum_n = sum(n)
    ) %>%
    arrange(desc(sum_n)) %>%
    add_column(letter = LETTERS[1:nrow(.)]) # nolint

namedColors <- c("Low-Loss & Low-Expense" = colorBlindBlack8[1], "Low-Loss & High-Expense" = colorBlindBlack8[2], "High-Loss & High-Expense" = colorBlindBlack8[8], "High-Loss & Low-Expense" = colorBlindBlack8[4], "Intermediate-Loss & Low-Expense" = colorBlindBlack8[5], "Intermediate-Loss & High-Expense" = colorBlindBlack8[6], "Low-Loss & Intermediate-Expense" = colorBlindBlack8[7], "High-Loss & Intermediate-Expense" = colorBlindBlack8[3], "Intermediate-Loss & Intermediate-Expense" = "black")

p <- plotEE(r_treatment$top, r_treatment$quantile)
fSaveImages(p, "efficient-economic", h = 10, w = 9)
p <- plotEE(r_treatment$big, r_treatment$quantile_big)
fSaveImages(p, "efficient-economic-big", h = 10, w = 9)
p <- plotEE(r_treatment$small, r_treatment$quantile_small)
fSaveImages(p, "efficient-economic-small", h = 10, w = 9)

r_treatment$tab <- r_treatment$top %>%
    createTable()
r_treatment$tab_big <- r_treatment$big %>%
    createTable()
r_treatment$tab_small <- r_treatment$small %>%
    createTable()

# Create dummy plot for m&m

dummy_df <- tibble(
    observed_costs = c(1.5, 1.5, 8.5, 8.5, 5, 5, 8.5, 1.5, 5),
    observed_loss = c(1.5, 8.5, 1.5, 8.5, 8.5, 1.5, 5, 5, 5),
    label_color = c("Low-Loss &\n Low-Expense", "High-Loss &\n Low-Expense", "Low-Loss &\n High-Expense", "High-Loss &\n High-Expense", "High-Loss &\n Intermediate-Expense", "Low-Loss &\n Intermediate-Expense", "Intermediate-Loss &\n High-Expense", "Intermediate-Loss &\n Low-Expense", "Intermediate-Loss &\n Intermediate-Expense"),
    year_loss = 5,
    year_costs = 5,
    loss_lower_ci = c(2, 7, 2, 7, 7, 2, 4.5, 4.5, 4.5),
    loss_upper_ci = c(3, 8, 3, 8, 8, 3, 5.5, 5.5, 5.5),
    observed_costs_se = 0.5
)

dummy_quantile <- tibble(
    # costs = quantile(dummy_df$observed_costs, probs = c(0.4, 0.6)),
    # loss = quantile(dummy_df$observed_loss, probs = c(0.4, 0.6))
    costs = c(3.5, 6.5),
    loss = c(3.5, 6.5)
)


p <- dummy_df %>%
    ggplot(aes(y = observed_costs, x = observed_loss, color = "black")) +
    # geom_vline(aes(xintercept = year_loss), color = "#0072B2", #linetype = "dashed") +
    # geom_hline(aes(yintercept = year_costs), color = "#0072B2", #linetype = "dashed") +
    geom_vline(aes(xintercept = 3.5), color = "#0072B2", linetype = "dashed") +
    geom_vline(aes(xintercept = 6.5), color = "#0072B2", linetype = "dashed") +
    geom_hline(aes(yintercept = 3.5), color = "#0072B2", linetype = "dashed") +
    geom_hline(aes(yintercept = 6.5), color = "#0072B2", linetype = "dashed") +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1, color = "black") +
    annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, size = 1, color = "black") +
    annotate("rect",
        xmin = 0, xmax = 5, ymin = 0, ymax = 5,
        fill = colorBlindBlack8[1]
    ) +
    annotate("rect",
        xmin = 0, xmax = 5, ymin = 5.01, ymax = 10,
        fill = colorBlindBlack8[2]
    ) +
    annotate("rect",
        xmin = 5.01, xmax = 10, ymin = 5.01, ymax = 10,
        fill = colorBlindBlack8[8]
    ) +
    annotate("rect",
        xmin = 5.01, xmax = 10, ymin = 0, ymax = 5,
        fill = colorBlindBlack8[4]
    ) +
    annotate("rect",
        xmin = dummy_quantile$loss[1], xmax = dummy_quantile$loss[2], ymin = 0, ymax = 5,
        fill = colorBlindBlack8[5]
    ) +
    annotate("rect",
        xmin = dummy_quantile$loss[1], xmax = dummy_quantile$loss[2], ymin = 6, ymax = 10,
        fill = colorBlindBlack8[6]
    ) +
    annotate("rect",
        ymin = dummy_quantile$costs[1], ymax = dummy_quantile$costs[2], xmin = 0, xmax = 5,
        fill = colorBlindBlack8[7]
    ) +
    annotate("rect",
        ymin = dummy_quantile$costs[1], ymax = dummy_quantile$costs[2], xmin = 6, xmax = 10,
        fill = colorBlindBlack8[3]
    ) +
    annotate("rect",
        ymin = 3.5, ymax = 6.5, xmin = 3.5, xmax = 6.5,
        fill = "black"
    ) +
    # geom_linerange(
    #    aes(ymin = observed_costs - observed_costs_se, ymax = #observed_costs + observed_costs_se),
    #    # color = "gray",
    #    alpha = 0.5,
    #    show.legend = FALSE
    # ) +
    # geom_linerange(
    #    aes(xmin = loss_lower_ci, xmax = loss_upper_ci),
    #    # color = "gray",
    #    alpha = 0.5,
    #    show.legend = FALSE
    # ) +
    geom_text(
        aes(label = label_color),
        fontface = "bold",
        show.legend = FALSE
    ) +
    # geom_point(size = 2) +
    # geom_curve(
    #    aes(x = 3, y = 4, xend = 4, yend = 5),
    #    arrow = arrow(),
    #    colour = "black",
    #    size = 1,
    #    angle = 90
    # ) +
    ## annotate("text", x = 2.8, y = 4, label = "Total Mean Expenses #per Year", hjust = 1) +
    # geom_curve(
    #    aes(x = 6, y = 6, xend = 5, yend = 7),
    #    arrow = arrow(),
    #    colour = "black",
    #    size = 1,
    #    angle = 90
    # ) +
    # annotate("text", x = 6, y = 5.8, label = "Total Observed Loss Rate per Year", hjust = 0) +
    # geom_segment(
    #    aes(x = 7, y = 4.1, xend = 7, yend = 4.5),
    #    arrow = arrow(),
    #    colour = "black",
    #    size = 1,
    #    angle = 90
    # ) +
    # geom_segment(
    #    aes(x = 6, y = 4, xend = 5.5, yend = 4),
    #    arrow = arrow(),
    #    colour = "black",
    #    size = 1,
    #    angle = 90
    # ) +

    # annotate("text", x = 6.1, y = 3.9, label = "Quantile 40% and 60%", hjust = 0) +

    geom_segment(
        aes(x = 3, y = 10.2, xend = 3.5, yend = 10.2),
        arrow = arrow(),
        colour = "black",
        size = 0.5,
        angle = 90
    ) +
    annotate("text", x = 2.9, y = 10.2, label = "33% Quantile", hjust = 1) +
    geom_segment(
        aes(x = 7, y = 10.2, xend = 6.5, yend = 10.2),
        arrow = arrow(),
        colour = "black",
        size = 0.5,
        angle = 90
    ) +
    annotate("text", x = 7.1, y = 10.2, label = "67% Quantile", hjust = 0) +
    geom_segment(
        aes(x = -0.2, y = 3, xend = -0.2, yend = 3.5),
        arrow = arrow(),
        colour = "black",
        size = 0.5,
        angle = 90
    ) +
    annotate("text", x = -0.2, y = 2.9, label = "33% Quantile", hjust = 1, angle = 90) +
    geom_segment(
        aes(x = -0.2, y = 7, xend = -0.2, yend = 6.5),
        arrow = arrow(),
        colour = "black",
        size = 0.5,
        angle = 90
    ) +
    annotate("text", x = -0.2, y = 7.1, label = "67% Quantile", hjust = 0, angle = 90) +
    ggplot2::scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        # limits = c(0, 10),
        labels = NULL,
        sec.axis = dup_axis()
    ) +
    ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        # limits = c(0, 10),
        labels = NULL
    ) +
    ggplot2::scale_color_manual(
        values = "white"
    ) +
    ylab("Mean Expenses/Colony [EUR] ± SE") +
    xlab("Mean Observed Colony Winter Loss [%] ± CI (95%)") +
    labs(color = "") +
    coord_cartesian(ylim = c(0, 10.2), xlim = c(-0.2, 10), clip = "off") +
    ggplot2::theme(
        legend.position = "bottom",
        axis.line.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        # panel.grid.major = element_line(color = "gray80", linetype = "dotted", size = 0.1)
    ) +
    guides(colour = guide_legend(override.aes = list(size = 8)))

fSaveImages(p, "efficient-economic-mm", h = 9, w = 9)
