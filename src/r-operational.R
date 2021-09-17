# Description -------------------------------------------------------------
# Singe Factor Analysis of Operation Size, Migratory and Cert. Org

# Links -------------------------------------------------------------------
# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# https://towardsdatascience.com/a-gentle-guide-to-statistics-in-r-ccb91cc1177e
# https://www.stata-journal.com/article.html?article=st0381
# https://rcompanion.org/rcompanion/d_06.html

# Chapter Operational Factors
r_operational <- list()

## Operation Size ----------
dfClean <- dfClean %>%
    mutate(
        operation = ifelse(hives_winter > 25, "> 25 Colonies", "<= 25 Colonies"),
        operation = as.factor(operation)
    )

r_operational$size$stats <- dfClean %>%
    group_by(year) %>%
    mutate(
        n = n(),
        colonies_total = sum(hives_winter),
        colonies_mean = round(mean(hives_winter)),
        colonies_median = median(hives_winter),
        colonies_max = max(hives_winter),
        colonies_label = glue::glue("Mean: {colonies_mean}, Median: {colonies_median}, Maximum: {colonies_max}"),
        year_label = glue::glue("20{year} - Beekeeper: {n}, Colonies: {colonies_total}")
    ) %>%
    ungroup() %>%
    glimpse()


r_operational$size$table <- dfClean %>%
    group_by(year, operation) %>%
    summarise(
        beekeeper = n(),
        colonies = sum(hives_winter),
        expenses_mean = format(round(mean(costs), 1), nsmall = 1),
        expenses_median = format(round(median(costs), 1), nsmall = 1)
    ) %>%
    ungroup() %>%
    glimpse()

### Distribution of Operation Size
r_operational$size$p <- r_operational$size$stats %>%
    ggplot2::ggplot(aes(hives_winter, fill = year)) +
    geom_bar(show.legend = FALSE) +
    geom_text(
        data = r_operational$size$stats %>% distinct(year, .keep_all = TRUE),
        aes(
            y = 45,
            x = 200,
            label = colonies_label
        ),
        size = 3
    ) +
    ylab("Beekeeper [n]") +
    xlab("Colonies [#]") +
    scale_color_manual(
        values = colorBlindBlack8[c(2, 4, 6)], aesthetics = "fill"
    ) +
    ggplot2::scale_x_continuous(
        breaks = scales::extended_breaks(n = 20)
    ) +
    ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks()
    ) +
    facet_wrap(~year_label, ncol = 1) +
    ggplot2::theme(
        panel.grid.major.y = element_line()
    )

fSaveImages(r_operational$size$p, "size-distr", h = 5)

### QQ Plot Operation Size - Expenses

r_operational$size$qq <- dfClean %>%
    fQQPlot(.) +
    facet_wrap(~operation)

fSaveImages(r_operational$size$qq, "size-qq")

### Statistics

r_operational$size$statistic_perm <- dfClean %>%
    # filter(operation %in% c("Ja", "Nein")) %>%
    # mutate(test = ifelse(op_cert_org_beek == "Ja", 1, 0) %>% as.factor()) %>%
    fPermTest(., "operation", stat = "diff in medians") %>%
    arrange(year_long)

# Save visual interpretation from permutation test
r_operational$size$statistic_perm %>%
    pmap(., fPlotPermutation) %>%
    patchwork::wrap_plots(.) %>%
    fSaveImages("size-permutation", w = 10)

# r_operational$size$statistic <- dfClean %>%
#    fKruskal(., sub = FALSE, col = "operation")

p <- dfClean %>%
    mutate(
        operation = stringr::str_remove(operation, " Colonies")
    ) %>%
    fPlotFactor(., "operation", r_operational$size$statistic_perm, c(50, 50, 50)) +
    xlab("Operation size / Number of colonies")
fSaveImages(p, "size-stats", w = 8, h = 5)

## Organic Beekeeper ----------
r_operational$organic$data <- dfClean %>%
    filter(op_cert_org_beek %in% c("Ja", "Nein")) %>%
    mutate(
        op_cert_org_beek = ifelse(op_cert_org_beek == "Ja", "Yes", "No"),
        op_cert_org_beek = forcats::as_factor(op_cert_org_beek)
    )

r_operational$organic$summary <- r_operational$organic$data %>%
    dplyr::count(op_cert_org_beek) %>%
    mutate(
        np = round(prop.table(n) * 100)
    ) %>%
    glimpse()

r_operational$organic$table <- r_operational$organic$data %>%
    group_by(year, op_cert_org_beek) %>%
    summarise(
        beekeeper = n(),
        colonies = sum(hives_winter),
        expenses_mean = format(round(mean(costs), 1), nsmall = 1),
        expenses_median = format(round(median(costs), 1), nsmall = 1)
    ) %>%
    ungroup() %>%
    glimpse()

# r_operational$organic$statistic <- r_operational$organic$data %>%
#    fKruskal(., sub = FALSE, col = "op_cert_org_beek")

r_operational$organic$statistic_perm <- r_operational$organic$data %>%
    fPermTest(., "op_cert_org_beek", stat = "diff in medians") %>%
    arrange(year_long)

# Save visual interpretation from permutation test
r_operational$organic$statistic_perm %>%
    pmap(., fPlotPermutation) %>%
    patchwork::wrap_plots(.) %>%
    fSaveImages("organic-permutation", w = 10)

p <- r_operational$organic$data %>%
    fPlotFactor(., "op_cert_org_beek", r_operational$organic$statistic_perm, c(50, 50, 50)) +
    xlab("Certified Organic Beekeeping Operation")
fSaveImages(p, "organic-stats", w = 8, h = 5)

## Migratory Beekeeper ----------
r_operational$migratory$data <- dfClean %>%
    filter(op_migratory_beekeeper %in% c("Ja", "Nein")) %>%
    mutate(
        op_migratory_beekeeper = ifelse(op_migratory_beekeeper == "Ja", "Yes", "No"),
        op_migratory_beekeeper = forcats::as_factor(op_migratory_beekeeper)
    )

r_operational$migratory$summary <- r_operational$migratory$data %>%
    dplyr::count(op_migratory_beekeeper) %>%
    mutate(
        np = round(prop.table(n) * 100)
    ) %>%
    glimpse()

r_operational$migratory$table <- r_operational$migratory$data %>%
    group_by(year, op_migratory_beekeeper) %>%
    summarise(
        beekeeper = n(),
        colonies = sum(hives_winter),
        expenses_mean = format(round(mean(costs), 1), nsmall = 1),
        expenses_median = format(round(median(costs), 1), nsmall = 1)
    ) %>%
    ungroup() %>%
    glimpse()

# r_operational$migratory$statistic <- r_operational$migratory$data %>%
#    fKruskal(., sub = FALSE, col = "op_migratory_beekeeper")

r_operational$migratory$statistic_perm <- r_operational$migratory$data %>%
    fPermTest(., "op_migratory_beekeeper", stat = "diff in medians") %>%
    arrange(year_long)

# Save visual interpretation from permutation test
r_operational$migratory$statistic_perm %>%
    pmap(., fPlotPermutation) %>%
    patchwork::wrap_plots(.) %>%
    fSaveImages("migratory-permutation", w = 10)

p <- r_operational$migratory$data %>%
    fPlotFactor(., "op_migratory_beekeeper", r_operational$migratory$statistic_perm, c(50, 50, 50)) +
    xlab("Migratory Beekeeping Operation")
fSaveImages(p, "migratory-stats", w = 8, h = 5)

# Save output as permutation takes quite a time
saveRDS(r_operational, "output/r-operational.rds")