# Description -------------------------------------------------------------
# Bland-Altmann plot variant to check difference and between
# estimate costs and survey costs

# Data --------------------------------------------------------------------
SUMMARY_blandAltmann <- DATA %>% 
  group_by(c_short_od) %>% 
  summarise(
    n     = n(),
    p     = round(n()*100/nrow(DATA),2),
    
    m_survey    = round(mean(costs), 2),
    med_survey  = round(median(costs), 2),
    
    m_estimate    = round(mean(t_estimated), 2),
    med_estimate  = round(median(t_estimated), 2),
    
    log_survey     = round(mean(log2(costs)), 2),
    log_estimate   = round(mean(log2(t_estimated)), 2)

  ) %>% 
  mutate(
    # log 2 transformation
    # 1 is double difference
    # -1 is half difference
    log_dif = log_survey - log_estimate
  ) %>% 
  filter(n >= 10) %>% 
  arrange(desc(n))

# Stats -------------------------------------------------------------------
ba.stats <- bland.altman.stats(SUMMARY_blandAltmann$log_survey, SUMMARY_blandAltmann$log_estimate)
ba.stats$label <- SUMMARY_blandAltmann$c_short_od

# Plot --------------------------------------------------------------------
limit_y <- round(
  ifelse( 
    max(ba.stats$diffs) > (-1*min(ba.stats$diffs)), 
    max(ba.stats$diffs), 
    -1*min(ba.stats$diffs) ), 
  digits = 1) + 0.2

labellogi <- (rank(ba.stats$diffs) < 3
 | rank(desc(ba.stats$diffs)) < 3 )
colorlogi <- ifelse(labellogi, colorBlindBlack8[8], colorBlindBlack8[1])

p <- ggplot() +
  aes(x = 2^(ba.stats$means), y = ba.stats$diffs) +
  geom_abline(
    aes(
      intercept = ba.stats$lines, 
      colour = I(c(colorBlindBlack8[3], colorBlindBlack8[5], colorBlindBlack8[3])), 
      slope = c(rep(0, 3))),
    show.legend = F
  ) +
  geom_abline(
    aes(
      intercept = ba.stats$CI.lines, 
      color = I(c(rep(colorBlindBlack8[3], 2), rep(colorBlindBlack8[5], 2), rep(colorBlindBlack8[3], 2))),
      slope = c(rep(0, 6))
    ),
    linetype = "dashed",
    alpha = 0.5,
    show.legend = F
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point(
    aes(size = SUMMARY_blandAltmann$n, color = I(colorlogi)), show.legend = F
  ) +
  geom_text(
    aes(
      x = 2^ba.stats$means[labellogi], 
      y = ba.stats$diffs[labellogi], 
      label = ba.stats$label[labellogi]
    ),
    size = 2,
    hjust = 1, # 0.5 left, 0 center, 1 right
    nudge_x = -0.2,
    check_overlap = T
    
  )+
  xlab("Mean of Survey and Estimates [Euro]") + ylab(TeX("Differences (Survey-Estimate) \\[$\\log_2$ Euro\\]")) +
  labs(size = "Answers [n]") + 
  scale_y_continuous(limits = c( -1*limit_y, limit_y), breaks = seq(-10, 10, 0.5)) +
  scale_x_continuous(limits = c(0, NA), breaks = seq(0, 100, 2))

fSaveImages("bland-altman", p, h = 4.5)
rm(ba.stats, limit_y, labellogi, colorlogi, p)
