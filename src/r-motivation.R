# Chapter Motivation for choosing treatment method
r_motivation <- list()

r_motivation$answers <- length(unique(dfMotivation$id))

# Operation Size and Common Selection
p <- dfData %>%
    filter(year == "20/21" & submitted == "Internet") %>%
    filter(!if_all(starts_with("motivation_"), ~ . == "Nein")) %>%
    select(starts_with("motivation_"), hives_winter) %>%
    mutate(
        operation = ifelse(hives_winter > 25, "> 25 Colonies", "<= 25 Colonies"),
        operation = as.factor(operation)
    ) %>%
    select(-hives_winter) %>%
    add_count(operation) %>%
    pivot_longer(starts_with("motivation_")) %>%
    drop_na(value) %>%
    filter(value == "Ja") %>%
    left_join(motivationList, by = c("name" = "cname")) %>%
    count(short, desc, name, operation, n) %>%
    glimpse() %>%
    group_by(operation, desc, short) %>%
    summarise(
        count = nn,
        perc = count / n * 100,
        perc_f = format(round(perc, 1), nsmall = 1)
    ) %>%
    arrange(desc(count)) %>%
    ungroup() %>%
    group_by(desc) %>%
    mutate(
        text_position = max(perc),
        order_sum = sum(perc)
    ) %>%
    ungroup() %>%
    mutate(
        short = forcats::fct_reorder(short, order_sum, .desc = FALSE),
        desc = stringr::str_trunc(desc, width = 47),
        desc = forcats::fct_reorder(desc, order_sum, .desc = FALSE),
    ) %>%
    ggplot2::ggplot(aes(x = desc, y = perc, fill = operation)) +
    geom_col(position = "dodge") +
    geom_text(
        aes(y = text_position + 2, label = paste0(perc_f, " (", count, ")")),
        # nudge_y = 30,
        hjust = 0,
        colour = "grey20",
        size = 3,
        position = position_dodge(width = .9)
    ) +
    ggplot2::scale_y_continuous(
        limits = c(0, 80),
        breaks = scales::breaks_pretty()
    ) +
    ggplot2::scale_fill_manual(values = c("#0072B2", "#009E73")) +
    xlab("") +
    ylab("Count [%]") +
    labs(fill = "Operation Size") +
    coord_flip(ylim = c(0, 85), expand = FALSE) +
    ggplot2::theme(
        legend.position = "top",
        # panel.grid.major.x = element_line(),
        # panel.grid.minor.x = element_line(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = ggplot2::element_text(color = "black")
    )

fSaveImages(p, "motivation-operation", h = 6.5)

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
        p = round(count / r_motivation$answers * 100, 1),
        percentage_of_participants = format(p, nsmall = 1)
    ) %>%
    arrange(desc(count)) %>%
    ungroup() %>%
    glimpse()

r_motivation$counts_state <- dfMotivation %>%
    filter(value == "Ja") %>%
    group_by(state) %>%
    mutate(
        state_count = length(unique(id)),
        state_de = state,
        state_en = stringr::str_replace_all(state_de, stateList),
        state_print = glue::glue("{state_en} (n={state_count})"),
    ) %>%
    ungroup() %>%
    add_count(short, name = "total_count") %>%
    add_count(state, name = "state_count") %>%
    group_by(desc, short, state_print, state_de) %>%
    summarise(
        total_count = first(total_count),
        state_count = first(state_count),
        p_state = round(n() / first(state_count) * 100, 1),
        p_state_label = format(round(p_state, 1), nsmall = 1)
    ) %>%
    arrange(desc(total_count)) %>%
    ungroup() %>%
    group_by(desc, short) %>%
    mutate(
        highest_lowest = max(p_state) == p_state | min(p_state) == p_state,
    ) %>%
    ungroup() %>%
    glimpse()

p2 <- r_motivation$counts_state %>%
    group_by(short) %>%
    filter(first(total_count) > 400) %>%
    ungroup() %>%
    mutate(
        # short = as.character(short),
        short = ifelse(short == "Side effects on bees", "Side effects\n on bees", short),
        short = forcats::fct_reorder(short, desc(total_count))
    ) %>%
    ggplot(aes(
        x = short, y = p_state, group = short,
        fill = state_print
    )) +
    geom_col() +
    geom_label(
        aes(
            label = p_state_label,
            alpha = highest_lowest
        ),
        label.padding = unit(0.15, "lines"),
        label.size = 0,
        fill = I("white"),
        size = 3,
        position = position_stack(vjust = 0.5),
        show.legend = FALSE
    ) +
    ylab("") +
    ggplot2::scale_fill_manual(
        values = c("#a19f9f", colorBlindBlack8[-1], "#60df71"),
        breaks = sort(unique(r_motivation$counts_state$state_print), decreasing = TRUE)
    ) +
    ggplot2::scale_color_identity() +
    labs(fill = "State") +
    xlab("Top 5 Motivation Answers") +
    ylab("Percentage of answers for each state") +
    ggplot2::theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = -15, r = 0, b = 25, l = 0, unit = "pt")),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
    )

fSaveImages(p2, "motivation-state", h = 5.5)

p <- r_motivation$counts %>%
    # dplyr::slice_max(count, n = 15) %>%
    mutate(
        short = forcats::fct_reorder(short, count, .desc = FALSE),
        desc = stringr::str_trunc(desc, width = 47),
        desc = forcats::fct_reorder(desc, count, .desc = FALSE),
    ) %>%
    ggplot2::ggplot(aes(x = desc, y = p)) +
    geom_col() +
    geom_text(
        aes(y = p + 2, label = paste0(percentage_of_participants, " (", count, ")")),
        # aes(label = paste0(percentage_of_participants, "% (", count, ")")),
        # nudge_y = 30,
        hjust = 0,
        colour = "grey20",
        size = 3.5
    ) +
    ggplot2::scale_y_continuous(
        limits = c(0, 80),
        breaks = scales::breaks_pretty()
    ) +
    xlab("") +
    ylab("Count [%]") +
    coord_flip(ylim = c(0, 85), expand = FALSE) +
    ggplot2::theme(
        legend.position = "none",
        # panel.grid.major.x = element_line(),
        # panel.grid.minor.x = element_line(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = ggplot2::element_text(color = "black")
    )

fSaveImages(p, "motivation-count", h = 5.5)


r_motivation$state_rank <- r_motivation$counts_state %>%
    group_by(state_de) %>%
    slice_max(n = 7, order_by = p_state) %>%
    mutate(rank = rank(desc(p_state))) %>%
    ungroup() %>%
    mutate(state = stringr::str_replace_all(state_de, stateList)) %>%
    select(state, desc, p_state_label, rank)