# Description -------------------------------------------------------------
# Operation Size is one of our Factors of Interest
# we would except that bigger companies spend less per colony
# we have two ways to group 1-20, 21-50 and > 50
# or 1-20, > 21, both would make sense

# Links -------------------------------------------------------------------
# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# https://towardsdatascience.com/a-gentle-guide-to-statistics-in-r-ccb91cc1177e
# https://www.stata-journal.com/article.html?article=st0381
# https://rcompanion.org/rcompanion/d_06.html

# Add Size Factor to DATA -------------------------------------------------
DATA <- DATA %>% 
  mutate(
    tri_size = case_when(
      hives_winter <= 20            ~ "1-20",
      between(hives_winter, 21, 50) ~ "21-50",
      hives_winter > 50             ~ ">50"
      ),
    two_size = case_when(
        hives_winter <= 20          ~ "1-20",
        hives_winter > 20           ~ ">20"
        ),
    tri_size = factor(tri_size, levels = c("1-20", "21-50", ">50")),
    two_size = factor(two_size, levels = c("1-20", ">20"))
  )

# Summary -----------------------------------------------------------------
SUMMARY_operation_trisize <- DATA %>% 
  group_by(tri_size, year) %>% 
  summarise(
    participants = n(),
    hives        = sum(hives_winter),
    mean_costs   = mean(costs),
    median_costs = median(costs)
  ) %>% 
  ungroup() %>% 
  arrange(year)


# QQ Plot -----------------------------------------------------------------
plotOperationQQ <- DATA %>% 
  filter(costs < 100) %>%
  ggplot(aes(sample = costs, group = year, color = year)) + 
  geom_qq(alpha=0.5) + geom_qq_line() + 
  theme_classic() + 
  ylab("Observed (Expenses / Colony) Quantile Distribution") +
  xlab("Theoretical Quantile Distribution") +
  facet_wrap(
    ~tri_size, ncol = 3,
    labeller = labeller(
      tri_size = function(x){return(paste(x, "Colonies"))},
      year = function(x){return(paste("Survey", x))}
    )
  ) +
  scale_color_manual(values = colorBlindBlack8[c(2,4)], name="Survey")

fSaveImages("qq-operation-size", plotOperationQQ)
rm(plotOperationQQ)

# Stats -------------------------------------------------------------------
STATS_operation_size <- list()

## Kruskal-Wallis ----------------------------------------------------------

STATS_operation_size$Kruskal <- DATA %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$costs, .x$tri_size))

STATS_operation_size$Effect <- DATA %>%
  split(.$year) %>%
  map(~ fEffektSize(.x$costs, .x$tri_size)) %>% 
  bind_rows(.id = "year")

## Dunn-Test ---------------------------------------------------------------

# pwc1819 <- DATA %>% filter(year == "18/19") %>% 
#   rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>% 
#   add_column(year = "18/19")
# pwc1920 <- DATA %>% filter(year == "19/20") %>% 
#   rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>% 
#   add_column(year = "19/20")
# Binding Position for Plotting of Significant Values
# resPWC <- rbind(pwc1819, pwc1920) %>% rstatix::add_xy_position(x = "tri_size")
# rm(pwc1819, pwc1920)


### Plot --------------------------------------------------------------------
facetLabels <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(STATS_operation_size$Kruskal, STATS_operation_size$Effect)
)
countLabel <- DATA %>% count(year, tri_size)

difLabel <- DATA %>% 
  group_by(tri_size, year) %>% 
  summarize(mean = mean(costs), median = median(costs)) %>% 
  ungroup() %>% 
  group_split(year, remove = F) %>%
  map_dfr(~ fPairwiseMM(.x))

plotOperationStat <- DATA %>% 
  filter(costs < 100) %>% 
  ggplot(
    aes(
       y = costs,
       x = tri_size,
       color = year
       )
    ) +
  geom_boxplot(show.legend = F) +
  stat_summary(
    fun = mean, geom = "point", show.legend = F, color = "black"
    ) +
  geom_text(
    data = countLabel, 
    mapping = aes(x = tri_size, label = paste0("n = ", n), y = 0),
    size = 2.5,
    vjust = 1.2,
    color = "gray"
  ) +
  theme_classic() + ylab("Expenses/Colony [Euro]") + xlab("Operation Size [Number Colonies]") +
  theme(
    axis.text.x = element_text(size = 12)
  ) +
  # labs(
  #   caption = "Black point indicating sample mean."
  # ) +
  ggsignif::geom_signif(
    data = difLabel,
    aes(
      xmin = xmin, xmax = xmax,
      annotations = paste(tex),
      y_position = c(78,70,62, 78,70,62)
      ),
    textsize = 3, color = "black", manual=TRUE, parse=TRUE
    ) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = c(seq(0,100,5))
  ) +
  scale_color_manual(
    values = colorBlindBlack8[c(2,4)], name="Survey"
    ) +
  geom_text(
    data = facetLabels, 
    aes(label=label, x = "21-50"), 
    parse=T, y = 100, inherit.aes = F,
    size = 3
    ) +
  facet_wrap(
  ~year,
  labeller = labeller(
    year = function(x){return(paste("Survey", x))}))


fSaveImages("stats-operation-size", plotOperationStat)
rm(facetLabels, countLabel, difLabel, plotOperationStat)


# Remove Treatment Effect  - Residuals -------------------------------------------------
aovSize <- DATA %>% 
  split(.$year) %>% 
  map(~aov(costs ~ t_short_od_lump, data = .x)) %>% 
  map(residuals) %>% 
  map_dfr(~bind_cols("residuals" = .x))

DATA$res_trisize <- aovSize$residuals
rm(aovSize)

STATS_operation_size$Kruskal_Residuals <- DATA %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$res_trisize, .x$tri_size))

STATS_operation_size$Effect_Rediuals <- DATA %>%
  split(.$year) %>%
  map(~ fEffektSize(.x$res_trisize, .x$tri_size)) %>% 
  bind_rows(.id = "year")


### Plot --------------------------------------------------------------------
countLabel_removedTreatment <- DATA %>% count(year, tri_size)
facetLabels_removedTreatment <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(STATS_operation_size$Kruskal_Residuals, STATS_operation_size$Effect_Rediuals)
)

difLabel_removedTreatment <- DATA %>% 
  group_by(tri_size, year) %>% 
  summarize(mean = mean(res_trisize), median = median(res_trisize)) %>% 
  ungroup() %>% 
  group_split(year, remove = F) %>%
  map_dfr(~ fPairwiseMM(.x))

plotOperationStat_removedTreatment <- DATA %>% 
  filter(res_trisize < 100) %>% 
  ggplot(
    aes(
       y = res_trisize,
       x = tri_size,
       color = year
     )
    ) +
  geom_boxplot(show.legend = F) +
  stat_summary(
    fun = mean, geom = "point", show.legend = F, color = "black"
  ) +
  geom_text(
    data = countLabel_removedTreatment, 
    mapping = aes(x = tri_size, label = paste0("n = ", n), y = -15),
    size = 2.5,
    vjust = 1.2,
    color = "gray"
  ) +
  theme_classic() + ylab("Residuals of costs ~ treatment methods") + xlab("Operation Size [Number Colonies]") +
  theme(
    axis.text.x = element_text(size = 12)
  ) +
  # labs(
  #   caption = "Black point indicating sample mean."
  # ) +
  ggsignif::geom_signif(
    data=difLabel_removedTreatment,
    aes(
      xmin=xmin, xmax=xmax,
      annotations = paste(tex),
      y_position = c(78,70,62, 78,70,62)
    ),
    textsize = 3, color = "black", manual=TRUE, parse=TRUE
  ) +
  scale_y_continuous(
    limits = c(-15,100),
    breaks = c(seq(-15,100,5))
  ) +
  scale_color_manual(
    values = colorBlindBlack8[c(2,4)], name="Survey"
  ) +
  geom_text(
    data = facetLabels_removedTreatment, 
    aes(label=label, x = "21-50"), 
    parse=T, y = 100, inherit.aes = F,
    size = 3
  ) +
  facet_wrap(
    ~year,
    labeller = labeller(
      year = function(x){return(paste("Survey", x))}))
plotOperationStat_removedTreatment
fSaveImages("stats-operation-size-residuals", plotOperationStat_removedTreatment)
rm(facetLabels_removedTreatment, countLabel_removedTreatment, difLabel_removedTreatment, plotOperationStat_removedTreatment)

# Extrapolation of Data ----------------------------------------------------
# https://www.ama.at/getattachment/4c3a7dfc-01a1-46a8-a2dd-4a876b7e8011/Imkereiprogramm-2020-2022-final.pdf?lang=de-AT

totalhives <- DATA %>% 
  group_by(year) %>% 
  summarise(
    t_hives = sum(hives_winter)
  )

cost_boot <- DATA %>% 
  group_by(year, tri_size) %>% 
  nest() %>% 
  mutate(
    boot = map_dfr(
      .x = data, 
      ~broom:::tidy.boot(boot(.x$costs, fBootMean, 10000), conf.int = T, type = "basic")
      )
  ) %>% 
  ungroup() %>% 
  select(-data) %>% 
  unnest_legacy() %>% 
  select(-boot)

extrapolated_df <- DATA %>% 
  left_join(., statsAut, by = c("year" = "year2")) %>% 
  left_join(., totalhives, by = c("year" = "year")) %>% 
  group_by(year, tri_size) %>% 
  summarise(
    hives = sum(hives_winter),
    hives_freq = sum(hives_winter)/t_hives[[1]],
    colonies = colonies[[1]],
    med   = median(costs),
    mad   = mad(costs)
  ) %>% 
  ungroup() %>%
  left_join(., cost_boot, by = c("year" = "year", "tri_size" = "tri_size")) %>%
  mutate(
    extrapolated_hives      = hives_freq*colonies,
    extrapolated_cost       = statistic * (hives_freq*colonies),
    extrapolated_cost_upper = conf.high * (hives_freq*colonies),
    extrapolated_cost_lower = conf.low * (hives_freq*colonies)
  )

SUMMARY_extrapolatedSum <- extrapolated_df %>% 
  group_by(year) %>% 
  summarise(
    total = sum(extrapolated_cost),
    colonies = sum(extrapolated_hives)
  ) %>% 
  mutate(
    label = paste(
      "Total",
      format(round(total), big.mark = ","), 
      "Euro for",
      format(round(colonies), big.mark = ","),
      "Colonies"
    )
  )

plotOperationStat_extrapolated <- extrapolated_df %>% 
  ggplot(aes(x = tri_size, y = extrapolated_cost, fill = year)) +
  geom_bar(stat = "identity") +
  geom_text(
    data = SUMMARY_extrapolatedSum, 
    aes(
      label = label, 
      x = "21-50"
      ), 
    parse = F, y = 1.5*10^6, inherit.aes = F,
    size = 3
  ) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE),
    breaks = seq(0,max(extrapolated_df$extrapolated_cost_upper), 100000)
    ) +
  theme_classic() + ylab("Extrapolated Total Costs [Euro]") + xlab("Operation Size [Number Colonies]") +
  geom_pointrange(aes(ymin = extrapolated_cost_lower, ymax = extrapolated_cost_upper)) +
  scale_fill_manual(
    values = colorBlindBlack8[c(2,4)], name="Survey"
  ) +
  facet_wrap(
    ~year,
    labeller = labeller(
      year = function(x){return(paste("Survey", x))}))

fSaveImages("stats-operation-size-extrapolated", plotOperationStat_extrapolated)

rm(plotOperationStat_extrapolated, totalhives, extrapolated_df, cost_boot)

