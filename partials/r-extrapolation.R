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
