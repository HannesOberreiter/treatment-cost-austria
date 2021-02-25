# Description -------------------------------------------------------------
# Migratory Beekeeper is probably a good indicator for professionalism

# Factor Data  ------------------------------------------------------------
# Rename and Relevel Factors
glevel <- c("Ja", "Nein", "Unsicher")
elevel <- c("Yes", "No", "Unsure")
subData <- DATA %>% 
  mutate(
    op_migratory_beekeeper = case_when(
      op_migratory_beekeeper == glevel[1] ~ elevel[1],
      op_migratory_beekeeper == glevel[2] ~ elevel[2],
      op_migratory_beekeeper == glevel[3] ~ elevel[3]
    )
  ) %>% 
  mutate(
    op_migratory_beekeeper = fct_relevel(op_migratory_beekeeper, elevel)
  )

# Summary -----------------------------------------------------------------
SUMMARY_migratory = subData %>% 
  group_by(op_migratory_beekeeper, year) %>% 
  summarise(
    participants = n(),
    hives = sum(hives_winter),
    mean_costs = mean(costs),
    median_costs = median(costs)
  ) %>% 
  ungroup() %>% 
  arrange(year)

# Subset Data -------------------------------------------------------------
subData <- subData %>% 
  filter(op_migratory_beekeeper %in% c("Yes", "No"))

# QQ Plot -----------------------------------------------------------------
plotQQ <- subData %>% 
  filter(costs < 100) %>% 
  ggplot(aes(sample=costs, group = year, color = year)) + 
  geom_qq(alpha=0.5) + geom_qq_line() + 
  theme_classic() + 
  ylab("Observed (Expenses / Colony) Quantile Distribution") +
  xlab("Theoretical Quantile Distribution") +
  facet_wrap(
    ~op_migratory_beekeeper, ncol = 2,
    labeller = labeller(
      op_migratory_beekeeper = function(x){return(paste("Migratory Beekeeper = ", x))},
      year = function(x){return(paste("Survey", x))}
    )
  ) +
  scale_color_manual(values = colorBlindBlack8[c(2,4)], name="Survey")

fSaveImages("qq-migratory", plotQQ)
rm(plotQQ)

# Stats -------------------------------------------------------------------
STATS_migratory <- list()

STATS_migratory$Kruskal <- subData %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$costs, .x$op_migratory_beekeeper))
STATS_migratory$Effect <- subData %>%
  split(.$year) %>%
  map_dfr(~ fEffektSize(.x$costs, .x$op_migratory_beekeeper))

## Plot --------------------------------------------------------------------
facetLabels <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(STATS_migratory$Kruskal, STATS_migratory$Effect)
)
countLabel <- subData %>% count(year, op_migratory_beekeeper)
difLabel <- subData %>% 
  group_by(op_migratory_beekeeper, year) %>% 
  summarize(
    mean = mean(costs), 
    median = median(costs)
    ) %>% 
  ungroup() %>% 
  group_split(year, remove = F) %>%
  map_dfr(~ fPairwiseMM(.x))

plotStat <- subData %>% 
  filter(costs < 200) %>% 
  ggplot(.,
         aes(
           y = costs,
           x = op_migratory_beekeeper,
           color = year
         )) +
  geom_boxplot(show.legend = F) +
  stat_summary(
    fun = mean, geom = "point", show.legend = F, color = "black"
  ) +
  geom_text(
    data = countLabel, 
    mapping = aes(x = op_migratory_beekeeper, label = paste0("n = ", n), y = 0),
    size = 2.5,
    vjust = 1.2,
    color = "gray"
  ) +
  theme_classic() + ylab("Expenses/Colony [Euro]") + xlab("Migratory Beekeeper") +
  theme(
    axis.text.x = element_text(size = 12)
  ) +
  # labs(
  #   caption = "Black point indicating sample mean."
  # ) +
  ggsignif::geom_signif(
    data=difLabel,
    aes(
      xmin=xmin, xmax=xmax,
      annotations = paste(tex),
      y_position = c(78, 78)
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
    aes(label=label, x = 1.5), 
    hjust = 0.5,
    parse=T, y = 100, inherit.aes = F,
    size = 3
  ) +
  facet_wrap(
    ~year,
    labeller = labeller(
      year = function(x){return(paste("Survey", x))}))

fSaveImages("stats-migratory", plotStat)
rm(facetLabels, countLabel, difLabel, plotStat)

# Only Same Treatment Method ----------------------------------------------
# Extract Treatment Methods used by migratory beekeepers
STATS_migratory$migratory_treatments <- subData %>% 
  filter(op_migratory_beekeeper == "Yes") %>% 
  select(c_short_od) %>% 
  unique() %>% 
  pull()

# generate second subset of data
subDataTreatment <- subData %>%
  filter(c_short_od %in% STATS_migratory$migratory_treatments)

STATS_migratory$Kruskal_Mig <- subDataTreatment %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$costs, as.factor(.x$op_migratory_beekeeper)))

STATS_migratory$Effect_Mig <- subDataTreatment %>%
  split(.$year) %>%
  map(~ fEffektSize(.x$costs, as.factor(.x$op_migratory_beekeeper))) %>% 
  bind_rows(.id = "year")

## Plot --------------------------------------------------------------------
countLabel_removedTreatment <- subDataTreatment %>%   
  count(year, op_migratory_beekeeper)

facetLabels_removedTreatment <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(STATS_migratory$Kruskal_Mig, STATS_migratory$Effect_Mig)
)

difLabel_removedTreatment <- subDataTreatment %>% 
  group_by(op_migratory_beekeeper, year) %>% 
  summarize(
    mean = mean(costs), 
    median = median(costs)
    ) %>% 
  ungroup() %>% 
  group_split(year, remove = F) %>%
  map_dfr(~ fPairwiseMM(.x))

plotMigratory_removedTreatment <- subDataTreatment %>%
  filter(costs < 100) %>% 
  ggplot(
    aes(
      y = costs,
      x = op_migratory_beekeeper,
      color = year
    )
  ) +
  geom_boxplot(show.legend = F) +
  stat_summary(
    fun = mean, geom = "point", show.legend = F, color = "black"
  ) +
  geom_text(
    data = countLabel_removedTreatment, 
    mapping = aes(x = op_migratory_beekeeper, label = paste0("n = ", n), y = 0),
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
    data=difLabel_removedTreatment,
    aes(
      xmin=xmin, xmax=xmax,
      annotations = paste(tex),
      y_position = c(78, 78)
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
    data = facetLabels_removedTreatment, 
    aes(label=label, x = 1.5), 
    parse=T, y = 100, inherit.aes = F,
    size = 3
  ) +
  facet_wrap(
    ~year,
    labeller = labeller(
      year = function(x){return(paste("Survey", x))}))

fSaveImages("stats-migratory-treatment", plotMigratory_removedTreatment)
rm(facetLabels_removedTreatment, countLabel_removedTreatment, difLabel_removedTreatment, plotMigratory_removedTreatment)
