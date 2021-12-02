# Chapter overview of treatment methods
r_treatment <- list()

r_treatment$months <- c(
    "Apr", "May", "Jun",
    "Jul", "Aug", "Sep",
    "Oct", "Nov", "Dec",
    "Jan", "Feb", "Mar"
)


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
            between(month, 8, 10) ~ "Winter",
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
        direction = -1
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