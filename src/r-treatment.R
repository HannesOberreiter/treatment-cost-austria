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
r_treatment$top <- dfClean %>%
    filter(costs != 0) %>%
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
        year_costs = mean(costs)
    ) %>%
    filter(nn >= 30) %>%
    ungroup() %>%
    group_by(c_short_od) %>%
    mutate(
        # observed_costs = fGeoMean(costs),
        # observed_costs_sd = fGeoSD(costs),
    ) %>%
    group_by(year, c_short_od, year_n, year_loss, year_costs, year_loss_ci) %>%
    summarise(
        n = n(),
        observed_loss = (sum(hives_lost) / sum(hives_winter)) * 100,
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
    unnest_wider(observed_loss_ci)

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
    add_column(letter = LETTERS[1:nrow(.)])

p <- r_treatment$top %>%
    left_join(r_treatment$letter, by = c("c_short_od")) %>%
    mutate(
        label_color = paste(efficient, economical, sep = " & ") %>% stringr::str_to_title(),
        year = paste0("20", year)
    ) %>%
    ggplot(aes(y = observed_costs, x = observed_loss, color = label_color)) +
    geom_vline(aes(xintercept = year_loss), color = "#0072B2", linetype = "dashed") +
    geom_hline(aes(yintercept = year_costs), color = "#0072B2", linetype = "dashed") +
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
    ggplot2::scale_color_manual(
        values = c("#D55E00", "#CC79A7", "#E69F00", "#009E73")
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
    guides(colour = guide_legend(override.aes = list(size = 8)))

fSaveImages(p, "efficient-economic", h = 10, w = 9)

r_treatment$tab <- r_treatment$top %>%
    left_join(r_treatment$letter) %>%
    arrange(letter) %>%
    select(letter, c_short_od, year, n, observed_costs_median, observed_costs, observed_costs_sd, observed_loss, loss_lower_ci, loss_upper_ci, efficient, economical)

# Create dummy plot for m&m

dummy_df <- tibble(
    observed_costs = c(2.5, 2.5, 7.5, 7.5),
    observed_loss = c(2.5, 7.5, 2.5, 7.5),
    label_color = c("Low-Loss & Low-Expense", "High-Loss & Low-Expense", "Low-Loss & High-Expense", "High-Loss & High-Expense"),
    year_loss = 5,
    year_costs = 5,
    loss_lower_ci = c(1.5, 6.5, 1.5, 6.5),
    loss_upper_ci = c(3.5, 8.5, 3.5, 8.5),
    observed_costs_se = 1
)

p <- dummy_df %>%
    ggplot(aes(y = observed_costs, x = observed_loss, color = label_color)) +
    geom_vline(aes(xintercept = year_loss), color = "#0072B2", linetype = "dashed") +
    geom_hline(aes(yintercept = year_costs), color = "#0072B2", linetype = "dashed") +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf, size = 1, color = "black") +
    annotate("segment", x = -Inf, xend = Inf, y = Inf, yend = Inf, size = 1, color = "black") +
    annotate("rect",
        xmin = 0, xmax = 5, ymin = 0, ymax = 5,
        alpha = .2,
        fill = "#009E73"
    ) +
    annotate("rect",
        xmin = 0, xmax = 5, ymin = 5.01, ymax = 10,
        alpha = .2,
        fill = "#E69F00"
    ) +
    annotate("rect",
        xmin = 5.01, xmax = 10, ymin = 5.01, ymax = 10,
        alpha = .2,
        fill = "#D55E00"
    ) +
    annotate("rect",
        xmin = 5.01, xmax = 10, ymin = 0, ymax = 5,
        alpha = .2,
        fill = "#CC79A7"
    ) +
    geom_linerange(
        aes(ymin = observed_costs - observed_costs_se, ymax = observed_costs + observed_costs_se),
        # color = "gray",
        alpha = 0.5,
        show.legend = FALSE
    ) +
    geom_linerange(
        aes(xmin = loss_lower_ci, xmax = loss_upper_ci),
        # color = "gray",
        alpha = 0.5,
        show.legend = FALSE
    ) +
    geom_text_repel(
        aes(label = label_color),
        fontface = "bold",
        show.legend = FALSE
    ) +
    geom_point(size = 2) +
    geom_curve(
        aes(x = 3, y = 4, xend = 4, yend = 5),
        arrow = arrow(),
        colour = "black",
        size = 1,
        angle = 90
    ) +
    annotate("text", x = 2.8, y = 4, label = "Total Mean Expenses per Year", hjust = 1) +
    geom_curve(
        aes(x = 6, y = 6, xend = 5, yend = 7),
        arrow = arrow(),
        colour = "black",
        size = 1,
        angle = 90
    ) +
    annotate("text", x = 6, y = 5.8, label = "Total Observed Loss Rate per Year", hjust = 0) +
    ggplot2::scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10),
        limits = c(0, 10),
        labels = scales::label_number(suffix = "\u0025", accuracy = 1),
        sec.axis = dup_axis()
    ) +
    ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(n = 8),
        limits = c(0, 10),
        labels = scales::label_dollar(prefix = "", suffix = "€")
    ) +
    ggplot2::scale_color_manual(
        values = c("#D55E00", "#CC79A7", "#E69F00", "#009E73")
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
    guides(colour = guide_legend(override.aes = list(size = 8)))

fSaveImages(p, "efficient-economic-mm", h = 9, w = 9)
