# Chapter Motivation for choosing treatment method
r_motivation <- list()

r_motivation$answers <- length(unique(dfMotivation$id))

# Table most common combinations
r_motivation$comb_list <- dfMotivation %>%
    filter(value == "Ja") %>%
    group_by(id) %>%
    summarise(
        motivation = paste0(desc, collapse = " || "),
        motivation_short = paste0(short, collapse = " || "),
        motivations = first(n)
    ) %>%
    group_by(motivation, motivation_short) %>%
    summarise(
        n = n(),
        motivations = first(motivations)
    ) %>%
    arrange(desc(n)) %>%
    glimpse() %>%
    ungroup()

# Table Total Motivation Counts
r_motivation$counts <- dfMotivation %>%
    filter(value == "Ja") %>%
    group_by(desc, short) %>%
    summarise(
        count = n(),
        percentage_of_participants = format(round(count / r_motivation$answers * 100, 1), nsmall = 1)
    ) %>%
    arrange(desc(count)) %>%
    ungroup() %>%
    glimpse()

r_motivation$counts_state <- dfMotivation %>%
    filter(value == "Ja") %>%
    add_count(short, name = "total_count") %>%
    group_by(desc, short, state) %>%
    summarise(
        total_count = first(total_count),
        p_state = n() / first(total_count) * 100,
        p_state_label = format(round(p_state, 1), nsmall = 1)
    ) %>%
    arrange(desc(total_count)) %>%
    ungroup() %>%
    glimpse()


p2 <- r_motivation$counts_state %>%
    group_by(short) %>%
    filter(first(total_count) > 400) %>%
    ungroup() %>%
    ggplot(aes(x = short, y = p_state, fill = state)) +
    geom_col() +
    geom_text(aes(label = p_state_label), size = 3, position = position_stack(vjust = 0.5)) +
    ylab("") +
    ggplot2::scale_fill_manual(
        values = c("#626161", colorBlindBlack8[-1], "#60df71")
    ) +
    labs(fill = "State") +
    xlab("Top 5 Motivation Answers") +
    ylab("Percentage of Austria total answers") +
    ggplot2::theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(vjust = 8),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
    )
fSaveImages(p2, "motivation-state", h = 5.5)

p <- r_motivation$counts %>%
    # dplyr::slice_max(count, n = 15) %>%
    mutate(
        short = forcats::fct_reorder(short, count, .desc = FALSE)
    ) %>%
    ggplot2::ggplot(aes(x = short, y = count)) +
    geom_col() +
    geom_text(
        aes(label = paste0(percentage_of_participants, "% (", count, ")")),
        nudge_y = 30,
        hjust = 0,
        colour = "grey20",
        size = 3.5
    ) +
    ggplot2::scale_y_continuous(
        breaks = scales::breaks_pretty()
    ) +
    xlab("") +
    ylab("Count (#)") +
    coord_flip(ylim = c(0, 1200)) +
    ggplot2::theme(
        legend.position = "none",
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = ggplot2::element_text(color = "black", margin = margin(t = 0, r = -20, b = 0, l = 0, unit = "pt"))
    )
fSaveImages(p, "motivation-count", h = 5.5)