# Description -------------------------------------------------------------
# Organic Beekepers are only allowed to use a subset of treatment methods
# they are also required document their treatments

# Factor Data  ------------------------------------------------------------
glevel <- c("Ja", "Nein", "Unsicher", "n/a")
elevel <- c("Yes", "No", "Unsure", "n/a")

subData <- DATA %>% 
  mutate(
    op_cert_org_beek = case_when(
      op_cert_org_beek == glevel[1] ~ elevel[1],
      op_cert_org_beek == glevel[2] ~ elevel[2],
      op_cert_org_beek == glevel[3] ~ elevel[3],
      TRUE ~ elevel[4],
    )
  ) %>% 
  mutate(
    op_cert_org_beek = fct_relevel(op_cert_org_beek, elevel)
  )

# Summary -----------------------------------------------------------------
SUMMARY_certorg <- subData %>% 
  group_by(op_cert_org_beek, year) %>% 
  summarise(
    participants = n(),
    hives        = sum(hives_winter),
    mean_costs   = mean(costs),
    median_costs = median(costs)
  ) %>% 
  ungroup() %>% 
  arrange(year)

# Subset Data -------------------------------------------------------------
subData <- subData %>% 
  filter(op_cert_org_beek %in% c("Yes", "No"))

# QQ Plot -----------------------------------------------------------------
plotQQ <- subData %>% 
  filter(costs < 100) %>% 
  ggplot(aes(sample=costs, group = year, color = year)) + 
  geom_qq(alpha=0.5) + 
  geom_qq_line() + 
  theme_classic() + 
  ylab("Observed (Expenses / Colony) Quantile Distribution") +
  xlab("Theoretical Quantile Distribution") +
  facet_wrap(
    ~op_cert_org_beek, ncol = 2,
    labeller = labeller(
      op_cert_org_beek = function(x){return(paste("Certified Organic = ", x))},
      year = function(x){return(paste("Survey", x))}
    )
  ) +
  scale_color_manual(values = colorBlindBlack8[c(2,4)], name="Survey")

fSaveImages("qq-cert-org", plotQQ)
rm(plotQQ)

# Stats -------------------------------------------------------------------
STATS_certorg <- list()

STATS_certorg$Kruskal <- subData %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$costs, .x$op_cert_org_beek))

STATS_certorg$Effect <- subData %>%
  split(.$year) %>%
  map_dfr(~ fEffektSize(.x$costs, .x$op_cert_org_beek))

## Plot --------------------------------------------------------------------
facetLabels <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(STATS_certorg$Kruskal, STATS_certorg$Effect)
)

countLabel <- subData %>% 
  count(year, op_cert_org_beek)

difLabel <- subData %>% 
  group_by(op_cert_org_beek, year) %>% 
  summarize(
    mean = mean(costs), 
    median = median(costs)
    ) %>% 
  ungroup() %>% 
  group_split(year, remove = F) %>%
  map_dfr(~ fPairwiseMM(.x))

plotStat <- subData %>% filter(costs < 200) %>% 
  ggplot(.,
         aes(
           y = costs,
           x = op_cert_org_beek,
           color = year
         )) +
  geom_boxplot(show.legend = F) +
  stat_summary(
    fun = mean, geom = "point", show.legend = F, color = "black"
  ) +
  geom_text(
    data = countLabel, 
    mapping = aes(x = op_cert_org_beek, label = paste0("n = ", n), y = 0),
    size = 2.5,
    vjust = 1.2,
    color = "gray"
  ) +
  theme_classic() + ylab("Expenses/Colony [Euro]") + xlab("Certified Organic Beekeeper") +
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

fSaveImages("stats-cert-org", plotStat)
rm(facetLabels, countLabel, difLabel, plotStat)

# Only Same Treatment Method ----------------------------------------------
# Extract Treatment Methods used by organic beekeepers
STATS_certorg$certorg_treatments <- subData %>% 
  filter(op_cert_org_beek == "Yes") %>% 
  select(c_short_od) %>% 
  unique() %>% 
  pull()

# generate second subset of data with only treatments used by organic beekeepers
subDataTreatment <- subData %>%
  filter(c_short_od %in% STATS_certorg$certorg_treatments)

STATS_certorg$Kruskal_cert <- subDataTreatment %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$costs, as.factor(.x$op_cert_org_beek)))

STATS_certorg$Effect_cert <- subDataTreatment %>%
  split(.$year) %>%
  map(~ fEffektSize(.x$costs, as.factor(.x$op_cert_org_beek))) %>% 
  bind_rows(.id = "year")

## Plot --------------------------------------------------------------------
countLabel_removedTreatment <- subDataTreatment %>%   
  count(year, op_cert_org_beek)

facetLabels_removedTreatment <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(STATS_certorg$Kruskal_cert, STATS_certorg$Effect_cert)
)

difLabel_removedTreatment <- subDataTreatment %>% 
  group_by(op_cert_org_beek, year) %>% 
  summarize(
    mean = mean(costs), 
    median = median(costs)
    ) %>% 
  ungroup() %>% 
  group_split(year, remove = F) %>%
  map_dfr(~ fPairwiseMM(.x))


plotOrg_removedTreatment <- subDataTreatment %>%
  filter(costs < 100) %>% 
  ggplot(
    aes(
      y = costs,
      x = op_cert_org_beek,
      color = year
    )
  ) +
  geom_boxplot(show.legend = F) +
  stat_summary(
    fun = mean, geom = "point", show.legend = F, color = "black"
  ) +
  geom_text(
    data = countLabel_removedTreatment, 
    mapping = aes(x = op_cert_org_beek, label = paste0("n = ", n), y = 0),
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

fSaveImages("stats-cert-org-treatment", plotOrg_removedTreatment)
rm(facetLabels_removedTreatment, countLabel_removedTreatment, difLabel_removedTreatment, plotOrg_removedTreatment)

