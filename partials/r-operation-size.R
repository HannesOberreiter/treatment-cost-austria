# Operation Size Analysis ####

# Links ####
# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# https://towardsdatascience.com/a-gentle-guide-to-statistics-in-r-ccb91cc1177e
# https://www.stata-journal.com/article.html?article=st0381
# https://rcompanion.org/rcompanion/d_06.html

# Generate Operation Size Groupings ####
DATA$tri_size <- "1-20"
DATA$tri_size[DATA$hives_winter > 20] <- "21-50"
DATA$tri_size[DATA$hives_winter > 50] <- ">50"
DATA = DATA %>% mutate(tri_size = factor(tri_size, levels = c("1-20", "21-50", ">50")))

# Summary ####
summaryOperation = DATA %>% 
  group_by(tri_size, year) %>% 
  summarise(
    participants = n(),
    hives = sum(hives_winter),
    mean_costs = mean(costs),
    median_costs = median(costs)
  ) %>% 
  ungroup() %>% 
  arrange(year)

# QQ Plot of Operation Size ####
plotOperationQQ <- ggplot(DATA[DATA$costs<100,], aes(sample=costs, group = year, color = year)) + 
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

# Kruskal-Wallis ####
resKruskal <- DATA %>%
  split(.$year) %>%
  map(~ fCoinKruskal(.x$costs, .x$tri_size))

resEffect <- DATA %>%
  split(.$year) %>%
  map_dfr(~ fEffektSize(.x$costs, .x$tri_size))

# Dunn-Test Pairwise ####
# pwc1819 <- DATA %>% filter(year == "18/19") %>% 
#   rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>% 
#   add_column(year = "18/19")
# pwc1920 <- DATA %>% filter(year == "19/20") %>% 
#   rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>% 
#   add_column(year = "19/20")
# Binding Position for Plotting of Significant Values
# resPWC <- rbind(pwc1819, pwc1920) %>% rstatix::add_xy_position(x = "tri_size")
# rm(pwc1819, pwc1920)

# Plot Statistical Test ####
# facetLabels <- tibble(
#   year = c("18/19", "19/20"),
#   label = c(
#   get_test_label(
#     resKruskall[1,], detailed = TRUE
#   ),
#   get_test_label(
#     resKruskall[2,], detailed = TRUE
#   ))
# )
facetLabels <- tibble(
  year = c("18/19", "19/20"),
  label = fCoinLabel(resKruskal, resEffect)
)
countLabel <- DATA %>% count(year, tri_size)
difLabel <- DATA %>% 
  group_by(tri_size, year) %>% 
  summarize(mean = mean(costs), median = median(costs)) %>% 
  ungroup() %>% 
  group_split(year, remove = F) %>%
  map_dfr(~ fPairwiseMM(.x))

plotOperationStat <- DATA %>% filter(costs < 200) %>% 
  ggplot(.,
         aes(
           y = costs,
           x = tri_size,
           color = year
         )) +
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
    data=difLabel,
    aes(
      xmin=xmin, xmax=xmax,
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
plotOperationStat
fSaveImages("stats-operation-size", plotOperationStat)
rm(facetLabels, countLabel, difLabel, plotOperationStat)