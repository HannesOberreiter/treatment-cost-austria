# Bland-Altmann
# log 2 transformation
# 1 is double difference
# -1 is half difference
d <- DATA %>% 
  group_by(t_short_od) %>% summarise(
    n    = n(),
    p    = round(n()*100/nrow(DATA),2),
    
    mc   = round(mean(costs), 2),
    medc = round(median(costs), 2),
    
    me   = round(mean(t_estimated), 2),
    mede = round(median(t_estimated), 2),
    
    lmc   = round(mean(log2(costs)), 2),
    lme   = round(mean(log2(t_estimated)), 2)
  ) %>% 
  mutate(
    log_dif = lmc-lme
  ) %>% 
  filter(n >= 15) %>% arrange(desc(n))

ba.stats <- bland.altman.stats(d$lmc, d$lme)
ba.stats$label <- d$t_short_od

limit_y = round(
  ifelse( 
    max(ba.stats$diffs) > (-1*min(ba.stats$diffs)), 
    max(ba.stats$diffs), 
    -1*min(ba.stats$diffs) ), 
  digits = 1)

labellogi = (rank(ba.stats$diffs) < 3
 | rank(desc(ba.stats$diffs)) < 3 )
colorlogi = ifelse(labellogi, colorBlindBlack8[8], colorBlindBlack8[1])
p <- ggplot() +
  aes(x = 2^(ba.stats$means), y = ba.stats$diffs) +
  geom_point(aes(size = d$n, color = I(colorlogi)), show.legend = F) +
  geom_label(
    aes(
      x = 2^ba.stats$means[labellogi], 
      y = ba.stats$diffs[labellogi], 
      label = ba.stats$label[labellogi]
    ),
    size = 2,
    hjust = 1, # 0.5 left, 0 center, 1 right
    nudge_x = -0.2
  )+
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
  
  xlab("Mean of Survey and Estimates [Euro]") + ylab(TeX("Differences (Survey-Estimate) \\[$\\log_2$ Euro\\]")) +
  labs(size = "Answers [n]") + 
  ylim(c( -1*limit_y, limit_y )) +
  xlim(0, NA) 

fSaveImages("bland-altman", p)
rm(d, ba.stats, limit_y, labellogi, colorlogi)
