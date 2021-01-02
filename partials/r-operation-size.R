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
summaryOperation = DATA %>% group_by(tri_size, year) %>% summarise(
  participants = n(),
  hives = sum(hives_winter),
  mean_costs = mean(costs),
  median_costs = median(costs)
) %>% ungroup() %>% arrange(year)

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
  scale_color_manual(values = colorBlindBlack8[c(2,4)], name="Survey") +
  labs(caption="1-20 Colonies, Survey 19/20, 2x Outliers >100 are hidden.")
plotOperationQQ

# Kruskall ####
resKruskal1819 <- DATA %>% filter(year == "18/19") %>% 
  rstatix::kruskal_test(costs ~ 0 + tri_size) %>% add_column(year = "18/19")
resKruskal1920 <- DATA %>% filter(year == "19/20") %>% 
  rstatix::kruskal_test(costs ~ 0 + tri_size) %>% add_column(year = "19/20")
resKruskall <- rbind(resKruskal1819, resKruskal1920)
rm(resKruskal1819, resKruskal1920)
#resKruskall

# Effect Size ####
# The interpretation values commonly in published literature are: 
# 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).
resEffect1819 <- DATA %>% filter(year == "18/19") %>% 
  kruskal_effsize(costs ~ 0 + tri_size) %>% add_column(year = "18/19")
resEffect1920 <- DATA %>% filter(year == "19/20") %>% 
  kruskal_effsize(costs ~ 0 + tri_size) %>% add_column(year = "19/20")
resEffect <- rbind(resEffect1819, resEffect1920)
rm(resEffect1819, resEffect1920)
#resEffect

# Dunn-Test Pairwise ####
pwc1819 <- DATA %>% filter(year == "18/19") %>% 
  rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>% 
  add_column(year = "18/19")
pwc1920 <- DATA %>% filter(year == "19/20") %>% 
  rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>% 
  add_column(year = "19/20")
# Binding Position for Plotting of Significant Values
resPWC <- rbind(pwc1819, pwc1920) %>% rstatix::add_xy_position(x = "tri_size")
rm(pwc1819, pwc1920)

# Plot Statistical Test ####
facetLabels <- tibble(
  year = c("18/19", "19/20"),
  label = c(
  get_test_label(
    resKruskall[1,], detailed = TRUE
  ),
  get_test_label(
    resKruskall[2,], detailed = TRUE
  ))
)
plotOperationStat <- DATA %>% filter(costs < 200) %>% 
  ggplot(.,
         aes(
           y = costs,
           x = tri_size,
           color = year
         )) +
  geom_boxplot(show.legend = F) +
  theme_classic() + ylab("Expenses/Colony [Euro]") + xlab("Operation Size [Number Colonies]") +
  theme(
    axis.text.x = element_text(size = 14)
  ) +
  labs(
    caption = str_replace(
      get_pwc_label(
        resPWC, type="text"
        ), 
      "pwc", "Pairwise Comparisons"
      )
    ) +
  ggsignif::geom_signif(
    data=resPWC,
    aes(
      xmin=xmin, xmax=xmax, 
      annotations=p.adj.signif, 
      y_position = c(75,80,87, 75,80,87), 
      group = p.adj
      ),
    textsize = 3, color = "black", manual=TRUE, parse=FALSE
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

#paste0(TeX("$\\eta^2$", output = "text"),round(resEffect$effsize[1],4))
