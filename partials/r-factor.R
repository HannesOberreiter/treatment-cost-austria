# Description -------------------------------------------------------------
# Singe Factor Analysis of Operation Size, Migratory and Cert. Org

# Links -------------------------------------------------------------------
# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# https://towardsdatascience.com/a-gentle-guide-to-statistics-in-r-ccb91cc1177e
# https://www.stata-journal.com/article.html?article=st0381
# https://rcompanion.org/rcompanion/d_06.html

# Functions ---------------------------------------------------------------
fQQPlot <- function(df){
  # First Part of the QQ Plot
  qq <- df %>% 
    filter(costs < 100) %>%
    ggplot(aes(sample = costs, group = year, color = year)) + 
    geom_qq(alpha=0.5) + geom_qq_line() + 
    theme_classic() + 
    ylab("Observed (Expenses / Colony) Quantile Distribution") +
    xlab("Theoretical Quantile Distribution") +
    scale_color_manual(values = colorBlindBlack8[c(2,4)], name="Survey")
  return(qq)
}

fSummaryFactor <- function(df, col){
  # simple summary function, which is the same for each factor
  result <- df %>% 
    group_by(.data[[col]], year) %>% 
    summarise(
      participants = n(),
      hives        = sum(hives_winter),
      mean_costs   = mean(costs),
      median_costs = median(costs)
    ) %>% 
    ungroup() %>% 
    arrange(year)
  return(result)
}

fKruskal <- function(df, sub, col){
  l <- list()
  
  l$Kruskal <- df %>%
    split(.$year) %>%
    map(~ fCoinKruskal(.x$costs, .x[[col]]))
  l$Effect <- l$Kruskal %>% 
    map(~ fEffektSize(.x))
  # l$Effect <- df %>%
  #   split(.$year) %>%
  #   map(~ fEffektSize(.x$costs, .x[[col]])) %>% 
  #   bind_rows(.id = "year")
  
  # l$Kruskal_Residuals <- df %>%
  #   split(.$year) %>%
  #   map(~ fCoinKruskal(.x[[paste0("res_", col)]], .x[[col]]))
  # l$Effect_Rediuals <- df %>%
  #   split(.$year) %>%
  #   map(~ fEffektSize(.x[[paste0("res_", col)]], .x[[col]])) %>% 
  #   bind_rows(.id = "year")
  
  l$Kruskal_Treatment <- sub %>%
    split(.$year) %>%
    map(~ fCoinKruskal(.x$costs, as.factor(.x[[col]])))
  l$Effect_Treatment <- l$Kruskal_Treatment %>% 
    map(~ fEffektSize(.x))
  # l$Effect_Treatment <- sub %>%
  #   split(.$year) %>%
  #   map(~ fEffektSize(.x$costs, as.factor(.x[[col]]))) %>% 
  #   bind_rows(.id = "year")
  
  return(l)
}

fPlotFactor <- function(df, col, krus, ef, sigY, textX){
  facetLabels <- tibble(
    year = c("18/19", "19/20"),
    label = fCoinLabel(krus, ef)
  )
  countLabel <- df %>% count(year, .data[[col]])
  
  difLabel <- df %>% 
    group_by(.data[[col]], year) %>% 
    summarize(mean = mean(costs), median = median(costs)) %>% 
    ungroup() %>% 
    group_split(year, remove = F) %>%
    map_dfr(~ fPairwiseMM(.x))
  
  plot <- df %>% 
    filter(costs < 100) %>% 
    ggplot(
      aes(
        y = costs,
        x = .data[[col]],
        color = year
      )
    ) +
    geom_boxplot(show.legend = F) +
    stat_summary(
      fun = mean, geom = "point", show.legend = F, color = "black"
    ) +
    geom_text(
      data = countLabel, 
      mapping = aes(x = .data[[col]], label = paste0("n = ", n), y = 0),
      size = 2.5,
      vjust = 1.2,
      color = "gray"
    ) +
    theme_classic() + 
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
        y_position = sigY
      ),
      textsize = 3, color = "black", manual=TRUE, parse=TRUE
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = c(seq(-15,100,5))
    ) +
    scale_color_manual(
      values = colorBlindBlack8[c(2,4)], name="Survey"
    ) +
    geom_text(
      data = facetLabels, 
      aes(label=label, x = textX), 
      parse=T, y = 100, inherit.aes = F,
      size = 3
    ) +
    ylab("Expenses/Colony [Euro]") + 
    facet_wrap(
      ~year,
      labeller = labeller(
        year = function(x){return(paste("Survey", x))}))
  
  return(plot)
}


# AOV ---------------------------------------------------------------
# we could use the residuals to remove treatment effect
# but linear function is not an good estimate for our data
# aovSize <- DATA %>% 
#   split(.$year) %>% 
#   map(~aov(costs ~ 0 + c_short_od, data = .x)) %>% 
#   map(residuals) %>% 
#   map_dfr(~bind_cols("residuals" = .x))
# DATA$residuals <- aovSize$residuals
# rm(aovSize)

# Operation Size ----------------------------------------------------------
# we would except that bigger companies spend less per colony
# we have two ways to group 1-20, 21-50 and > 50
# or 1-20, > 21, both would make sense

## Add Size Factor to DATA -------------------------------------------------
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

## Generate Main List ------------------------------------------------------
STATS_operation_size <- list()

## Summary -----------------------------------------------------------------
STATS_operation_size$Summary <- fSummaryFactor(DATA, "tri_size")

## QQ -----------------------------------------------------------------
plotQQ <- fQQPlot(DATA) +
  facet_wrap(
    ~tri_size, ncol = 3,
    labeller = labeller(
      tri_size = function(x){return(paste(x, "Colonies"))},
      year = function(x){return(paste("Survey", x))}
    )
  )
fSaveImages("qq-operation-size", plotQQ)
rm(plotQQ)

## Subset ------------------------------------------------------------------
# Extract Treatment Methods used by defined Factor
STATS_operation_size$treatments <- DATA %>% 
  filter(tri_size == ">50") %>% 
  select(c_short_od) %>% 
  unique() %>% 
  pull()

# generate second subset of data
STATS_operation_size$subDataTreatment <- DATA %>%
  filter(c_short_od %in% STATS_operation_size$treatments)

## Statistics --------------------------------------------------------------
STATS_operation_size <- c(
  STATS_operation_size, 
  fKruskal(DATA, STATS_operation_size$subDataTreatment, "tri_size")
  )

## Plots -------------------------------------------------------------------
plot_org <- fPlotFactor(
  DATA, 
  "tri_size", 
  STATS_operation_size$Kruskal, 
  STATS_operation_size$Effect,
  c(78,70,62, 78,70,62),
  "21-50"
  ) +
  xlab("Operation Size [Number Colonies]")

plot_treatment <- fPlotFactor(
  STATS_operation_size$subDataTreatment, 
  "tri_size", 
  STATS_operation_size$Kruskal_Treatment, 
  STATS_operation_size$Effect_Treatment,
  c(78,70,62, 78,70,62),
  "21-50"
  ) +
  xlab("Operation Size [Number Colonies]")

fSaveImages("stats-operation-size", plot_org)
fSaveImages("stats-operation-size-treatment", plot_treatment)

rm(plot_org, plot_treatment)

# Migratory Beekeeper ----------------------------------------------------
# Migratory Beekeeper is probably a good indicator for professionalism

## Factor Data  ------------------------------------------------------------
# Rename and Relevel Factors
glevel <- c("Ja", "Nein", "Unsicher")
elevel <- c("Yes", "No", "Unsure")
DATA <- DATA %>% 
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

## Generate Main List ------------------------------------------------------
STATS_migratory <- list()

## Summary -----------------------------------------------------------------
STATS_migratory$Summary <- fSummaryFactor(DATA, "op_migratory_beekeeper")

## SubSet Yes/No ------------------------------------------------------------------
STATS_migratory$subData <- DATA %>% 
  filter(op_migratory_beekeeper %in% c("Yes", "No"))

## QQ -----------------------------------------------------------------
plotQQ <- fQQPlot(STATS_migratory$subData) +
  facet_wrap(
    ~op_migratory_beekeeper, ncol = 2,
    labeller = labeller(
      op_migratory_beekeeper = function(x){return(paste("Migratory Beekeeper = ", x))},
      year = function(x){return(paste("Survey", x))}
    )
  )
fSaveImages("qq-migratory", plotQQ)
rm(plotQQ)

## Subset ------------------------------------------------------------------
# Extract Treatment Methods used by defined Factor
STATS_migratory$treatments <- STATS_migratory$subData %>% 
  filter(op_migratory_beekeeper == "Yes") %>% 
  select(c_short_od) %>% 
  unique() %>% 
  pull()
# generate second subset of data
STATS_migratory$subDataTreatment <- STATS_migratory$subData %>%
  filter(c_short_od %in% STATS_migratory$treatments)

## Statistics --------------------------------------------------------------
STATS_migratory <- c(
  STATS_migratory, 
  fKruskal(STATS_migratory$subData, STATS_migratory$subDataTreatment, "op_migratory_beekeeper")
)

## Plots -------------------------------------------------------------------
plot_org <- fPlotFactor(
  STATS_migratory$subData, 
  "op_migratory_beekeeper", 
  STATS_migratory$Kruskal, 
  STATS_migratory$Effect,
  c(78, 78),
  1.5
  ) +
  xlab("Migratory Beekeeper")

plot_treatment <- fPlotFactor(
  STATS_migratory$subDataTreatment, 
  "op_migratory_beekeeper", 
  STATS_migratory$Kruskal_Treatment, 
  STATS_migratory$Effect_Treatment,
  c(78, 78),
  1.5
) +
  xlab("Migratory Beekeeper")

fSaveImages("stats-migratory", plot_org)
fSaveImages("stats-migratory-treatment", plot_treatment)

rm(plot_org, plot_treatment)

# Cert. Org. Beekeeper ----------------------------------------------------
# Organic Beekepers are only allowed to use a subset of treatment methods
# they are also required document their treatments

## Factor Data  ------------------------------------------------------------
glevel <- c("Ja", "Nein", "Unsicher", "n/a")
elevel <- c("Yes", "No", "Unsure", "n/a")

DATA <- DATA %>% 
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

## Generate Main List ------------------------------------------------------
STATS_certorg <- list()

## Summary -----------------------------------------------------------------
STATS_certorg$Summary <- fSummaryFactor(DATA, "op_cert_org_beek")

## SubSet Yes/No ------------------------------------------------------------------
STATS_certorg$subData <- DATA %>% 
  filter(op_cert_org_beek %in% c("Yes", "No"))

## QQ -----------------------------------------------------------------
plotQQ <- fQQPlot(STATS_certorg$subData) +
  facet_wrap(
    ~op_cert_org_beek, ncol = 2,
    labeller = labeller(
      op_cert_org_beek = function(x){return(paste("Certified Organic = ", x))},
      year = function(x){return(paste("Survey", x))}
    )
  )
fSaveImages("qq-cert-org", plotQQ)
rm(plotQQ)

## Subset ------------------------------------------------------------------
# Extract Treatment Methods used by defined Factor
STATS_certorg$treatments <- STATS_certorg$subData %>% 
  filter(op_cert_org_beek == "Yes") %>% 
  select(c_short_od) %>% 
  unique() %>% 
  pull()
# generate second subset of data
STATS_certorg$subDataTreatment <- STATS_certorg$subData %>%
  filter(c_short_od %in% STATS_certorg$treatments)

## Statistics --------------------------------------------------------------
STATS_certorg <- c(
  STATS_certorg, 
  fKruskal(STATS_certorg$subData, STATS_certorg$subDataTreatment, "op_cert_org_beek")
)

## Plots -------------------------------------------------------------------
plot_org <- fPlotFactor(
  STATS_certorg$subData, 
  "op_cert_org_beek", 
  STATS_certorg$Kruskal, 
  STATS_certorg$Effect,
  c(78, 78),
  1.5
) +
  xlab("Certified Organic Beekeeper")

plot_treatment <- fPlotFactor(
  STATS_certorg$subDataTreatment, 
  "op_cert_org_beek", 
  STATS_certorg$Kruskal_Treatment, 
  STATS_certorg$Effect_Treatment,
  c(78, 78),
  1.5
) +
  xlab("Certified Organic Beekeeper")

fSaveImages("stats-cert-org", plot_org)
fSaveImages("stats-cert-org-treatment", plot_treatment)

rm(plot_org, plot_treatment)

# Inactive Code Chunks --------------------------------------------------
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

