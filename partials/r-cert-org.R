# Certified Organic Analysis ####

glevel = c("Ja", "Nein", "Unsicher", "n/a")
elevel = c("Yes", "No", "Unsure", "n/a")

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

# Summary ####
summaryCertOrg = subData %>% 
  group_by(op_cert_org_beek, year) %>% 
  summarise(
    participants = n(),
    hives = sum(hives_winter),
    mean_costs = mean(costs),
    median_costs = median(costs)
  ) %>% 
  ungroup() %>% 
  arrange(year)

# Only Yes No for further analysis
subData <- subData %>% 
  filter(op_cert_org_beek %in% c("Yes", "No"))

# QQ Plot ####
plotQQ <- subData %>% 
  filter(costs < 100) %>% 
  ggplot(aes(sample=costs, group = year, color = year)) + 
  geom_qq(alpha=0.5) + geom_qq_line() + 
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
plotQQ
fSaveImages("qq-cert-org", plotQQ)
rm(plotQQ)

# Kruskal-Wallis ####
resKruskal <- subData %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$costs, .x$op_cert_org_beek))

resEffect <- subData %>%
  split(.$year) %>%
  map_dfr(~ fEffektSize(.x$costs, .x$op_cert_org_beek))

facetLabels <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(resKruskal, resEffect)
)
countLabel <- subData %>% count(year, op_cert_org_beek)
difLabel <- subData %>% 
  group_by(op_cert_org_beek, year) %>% 
  summarize(mean = mean(costs), median = median(costs)) %>% 
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
plotStat
fSaveImages("stats-cert-org", plotStat)
rm(facetLabels, countLabel, difLabel, plotStat)