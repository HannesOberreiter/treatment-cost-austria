# PCA ---------------------------------------------------------------------

fGLM <- function(d){
  GLM.FULL <- glm(
    cbind( hives_lost_e, hives_spring_e ) ~ c_short_od_lump,
    family = quasibinomial( link = "logit" ), 
    data = d, na.action = na.omit
  )
  SUMMARY.FULL <- summary( GLM.FULL )
  print( SUMMARY.FULL ) 
  # F-Test
  ANOVA.FULL <- anova( GLM.FULL, test = "F" )
  print( ANOVA.FULL )
  # Chi-Sqr Test
  ANOVA.CHISQ <- anova( GLM.FULL, test = "Chisq" )
  print( ANOVA.CHISQ )
  
  # Calculate odds via model for factors  
  DFRESIDUAL.FULL <- SUMMARY.FULL$df.residual
  VALUES.FULL <- predict(
    GLM.FULL, 
    tibble(c_short_od_lump = levels(GLM.FULL$data$c_short_od_lump)),
    type = "link", se.fit = T
  )
  
  # Approximate 95% CIs for the log odds & upper and lower limit (NOT WITH REFERENCE REGION, Dunno)
  CACHE.ODDS <- VALUES.FULL$fit
  CACHE.LOWERLIM <- VALUES.FULL$fit - qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  CACHE.UPPERLIM <- VALUES.FULL$fit + qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  # Add Prob. to our plot matrix
  CACHE.ODDS =inv.logit( CACHE.ODDS ) * 100 
  CACHE.LOWERLIM = inv.logit( CACHE.LOWERLIM ) * 100 
  CACHE.UPPERLIM = inv.logit( CACHE.UPPERLIM ) * 100 
  
  CACHE.BIND <- tibble(
    levels = levels(GLM.FULL$data$c_short_od_lump),
    lowerlim = CACHE.LOWERLIM,
    middle = CACHE.ODDS,
    upperlim = CACHE.UPPERLIM
  )
  return(CACHE.BIND)
}


pc1920 <- DATA %>% 
  filter(year == "19/20") %>% 
  mutate(
    t_short_od_lump = fct_drop(t_short_od_lump),
    c_short_od_lump = fct_drop(c_short_od_lump)
  )

pc1920 <- pc1920 %>% 
  group_by(year, c_short_od_lump) %>% 
  summarise(
    n = n(),
    mean = mean(costs),
    median = median(costs),
    mad = mad(costs),
    sd = sd(costs)
  ) %>% 
  bind_cols(fGLM(pc1920))

pc1819 <- DATA %>% 
  filter(year == "18/19") %>% 
  mutate(
    t_short_od_lump = fct_drop(t_short_od_lump),
    c_short_od_lump = fct_drop(c_short_od_lump)
  )
pc1819 <- pc1819 %>% 
  group_by(year, c_short_od_lump) %>% 
  summarise(
    n = n(),
    mean = mean(costs),
    median = median(costs),
    mad = mad(costs),
    sd = sd(costs)
  ) %>% 
  bind_cols(fGLM(pc1819))

pc <- bind_rows(pc1920, pc1819)

# breakline for costs both years together? Maybe makes more sense
dc_sum <- pc %>% summarise(
  total_mean = mean(DATA$costs),
  total_median = median(DATA$costs),
  total_middle = mean(middle)
)

pc <- pc %>% left_join(dc_sum, by = c("year" = "year"))

pc <- pc %>% mutate(
  type = case_when(
    median <= total_median & middle < total_middle ~ "Cheap and Good",
    median > total_median & middle > total_middle ~ "Expensive and Bad",
    median <= total_median & middle > total_middle ~ "Cheap and Bad",
    median > total_median & middle < total_middle ~ "Expensive and Good"
  ),
  type_mean = case_when(
    mean <= total_mean & middle < total_middle ~ "Cheap and Good",
    mean > total_mean & middle > total_middle ~ "Expensive and Bad",
    mean <= total_mean & middle > total_middle ~ "Cheap and Bad",
    mean > total_mean & middle < total_middle ~ "Expensive and Good"
  )
)

pcPlot <- pc %>% 
  #ggplot(aes(x = mean, y = middle, color = type_mean)) +
  ggplot(aes(x = median, y = middle, color = type)) +
  geom_hline(data = dc_sum, aes(yintercept = total_middle), size = 1, color = "black") +
  #geom_vline(data = dc_sum, aes(xintercept = total_mean), size = 1, color = "red") +
  geom_vline(data = dc_sum, aes(xintercept = total_median), size = 1, color = "black") +
  geom_label_repel(aes(label=c_short_od_lump), max.overlaps = 30, show.legend = F) +
  geom_point(show.legend = T) +
  geom_errorbar(aes(ymin = lowerlim, ymax = upperlim), show.legend = F, alpha = 0.2) +
  #geom_errorbarh(aes(xmin = mean-sd, xmax = mean+sd), show.legend = F, alpha = 0.2) +
  geom_errorbarh(aes(xmin = median-mad, xmax = median+mad), show.legend = F, alpha = 0.2) +
  scale_y_continuous(
    breaks = seq(0, 50, 5)
  ) +
  scale_x_continuous(
    breaks = seq(0, 15, 1)
  ) +
  scale_color_manual(
    values = colorBlindBlack8, name="Survey"
  ) +
  coord_cartesian(xlim = c(0,15), ylim = c(0,40), expand = FALSE) +
  xlab("Expenses/Colony [Euro]") +
  ylab("Loss rates [%]") +
  facet_wrap(~ year, ncol = 1, scales = "free_x")

fSaveImages("stats-loss-cost-plot", pcPlot, 12, 12)










