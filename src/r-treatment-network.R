# Separate file for overview of treatment networks
r_network <- list()
library(tidygraph)
library(ggraph)
# Select data from original dataset
# filter columns with less than 30 answers in total over all three years

r_network[["raw"]] <-
    dfData %>%
    select(contains("totalyn")) %>%
    select(-contains("vcount")) %>%
    filter(rowSums(.) != 0) %>%
    select(where(~ sum(.x) >= 30))
# Create adjacency Matrix
# Diagonals are how often some treatments were applied
r_network[["matrix"]] <- t(as.matrix(r_network[["raw"]])) %*% as.matrix(r_network[["raw"]])
r_network[["diag"]] <- tibble::as_tibble(diag(r_network[["matrix"]]), rownames = "name") %>%
    mutate(
        ttotal = stringr::str_extract(name, ".*_total")
    ) %>%
    left_join(treatmentList)

r_network[["data"]] <- as_tbl_graph(r_network[["matrix"]]) %>%
    activate(edges) %>%
    convert(to_simple, .clean = FALSE) %>%
    mutate(
        weight = map_dbl(`.orig_data`, ~ .x %>% pull(weight))
    ) %>%
    arrange(weight) %>%
    activate(nodes) %>%
    mutate(
        centrality = centrality_betweenness(),
        season = case_when(
            stringr::str_detect(name, "spring") ~ "Spring",
            stringr::str_detect(name, "summer") ~ "Summer",
            stringr::str_detect(name, "winter") ~ "Winter",
            TRUE ~ "-"
        ),
    ) %>%
    left_join(diag_temp)
p <- r_network[["data"]] %>%
    ggraph("stress") +
    geom_edge_link0(aes(edge_width = weight, alpha = weight, color = weight)) +
    geom_node_point(aes(fill = season, size = value), shape = 21) +
    geom_node_label(aes(label = tshort, color = season), repel = TRUE, show.legend = FALSE) +
    # geom_node_text(aes(label = tshort, color = season), repel=TRUE ) +
    ggplot2::coord_fixed() +
    scale_edge_width(
        breaks = c(30, 50, 100, 500, 1000),
        trans = scales::pseudo_log_trans(sigma = 10),
        range = c(0, 3),
        name = "Combination [n]",
        guide = "none"
    ) +
    ggraph::scale_edge_color_viridis(
        breaks = scales::pretty_breaks(),
        option = "plasma",
        name = "Combination [n]"
    ) +
    ggraph::scale_edge_alpha(name = "Combination [n]", guide = "none") +
    scale_size(
        breaks = c(50, 100, 500, 1000, 2000),
        range = c(1, 6)
    ) +
    scale_fill_manual(
        values = c("Spring" = "#CC79A7", "Summer" = "#D55E00", "Winter" = "#56B4E9")
    ) +
    scale_color_manual(
        values = c("Spring" = "#CC79A7", "Summer" = "#D55E00", "Winter" = "#56B4E9")
    ) +
    labs(color = "Season", fill = "Season", size = "Total [n]") +
    # theme_graph() +
    theme() +
    guides(
        fill = guide_legend(override.aes = list(size = 5))
    )

fSaveImages(p, "treatment-network", w = 7, h = 5.5)


# View(as_tibble(r_network[["data"]]))
#
# View(r_network[["data"]] %>%
#    activate("edges") %>%
#    as_tibble())