# Extrapolation of Data ----------------------------------------------------
# https://www.ama.at/getattachment/4c3a7dfc-01a1-46a8-a2dd-4a876b7e8011/Imkereiprogramm-2020-2022-final.pdf?lang=de-AT

r_extrapolation <- list()

dfClean$t_estimated

# Survey Extrapolation
set.seed(2021)
r_extrapolation$data <- dfClean %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    colonies_survey = map(data, ~ sum(.x$hives_winter)),
    boot = map(
      .x = data,
      # Resampling Simulations
      # https://www.tandfonline.com/doi/abs/10.1080/00949659208811373
      ~ boot(data = .x$costs, statistic = fBootMean, R = 1000, parallel = "multicore", sim = "ordinary") %>%
        broom::tidy(conf.int = TRUE) %>%
        add_column(type = "Arithmetic Mean")
    ),
    geom_boot = map(
      .x = data,
      ~ boot(data = .x$costs, statistic = fBootGeomMean, R = 1000, parallel = "multicore", sim = "balanced") %>%
        broom::tidy(conf.int = TRUE) %>%
        add_column(type = "Geometric Mean")
    ),
    boot_sum = list(bind_rows(boot, geom_boot))
  ) %>%
  unnest(colonies_survey, boot_sum) %>%
  select(-c("data", "boot", "geom_boot")) %>%
  arrange(type, year) %>%
  left_join(statsAut, by = c("year" = "year2")) %>%
  select(-year.y, beekeeper) %>%
  mutate(
    year = paste0("20", year),
    survey_estimate = statistic * colonies_survey,
    survey_estimate_lower = conf.low * colonies_survey,
    survey_estimate_upper = conf.high * colonies_survey,
    extrapolation_estimate = statistic * colonies,
    extrapolation_estimate_lower = conf.low * colonies,
    extrapolation_estimate_upper = conf.high * colonies,
    extrapolation_estimate_100k = statistic * 100000,
    extrapolation_estimate_lower_100k = conf.low * 100000,
    extrapolation_estimate_upper_100k = conf.high * 100000,
  )

r_extrapolation$data <- r_extrapolation$data %>%
  left_join(dfClean %>%
    group_by(year) %>%
    summarise(
      year = paste0("20", first(year)),
      survey_real = sum(hives_winter * costs)
    ))

# Estimate Extrapolation
r_extrapolation$data_estimate <- dfClean %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    colonies_survey = map(data, ~ sum(.x$hives_winter)),
    boot = map(
      .x = data,
      # Resampling Simulations
      # https://www.tandfonline.com/doi/abs/10.1080/00949659208811373
      ~ boot(data = .x$t_estimated, statistic = fBootMean, R = 1000, parallel = "multicore", sim = "ordinary") %>%
        broom::tidy(conf.int = TRUE) %>%
        add_column(type = "Arithmetic Mean - Estimate")
    ),
    geom_boot = map(
      .x = data,
      ~ boot(data = .x$t_estimated, statistic = fBootGeomMean, R = 1000, parallel = "multicore", sim = "balanced") %>%
        broom::tidy(conf.int = TRUE) %>%
        add_column(type = "Geometric Mean - Estimate")
    ),
    boot_sum = list(bind_rows(boot, geom_boot))
  ) %>%
  unnest(colonies_survey, boot_sum) %>%
  select(-c("data", "boot", "geom_boot")) %>%
  arrange(type, year) %>%
  left_join(statsAut, by = c("year" = "year2")) %>%
  select(-year.y, beekeeper) %>%
  mutate(
    year = paste0("20", year),
    survey_estimate = statistic * colonies_survey,
    survey_estimate_lower = conf.low * colonies_survey,
    survey_estimate_upper = conf.high * colonies_survey,
    extrapolation_estimate = statistic * colonies,
    extrapolation_estimate_lower = conf.low * colonies,
    extrapolation_estimate_upper = conf.high * colonies,
  )

r_extrapolation$data_estimate <- r_extrapolation$data %>%
  left_join(dfClean %>%
    group_by(year) %>%
    summarise(
      year = paste0("20", first(year)),
      survey_real = sum(hives_winter * costs)
    ))

p <- r_extrapolation$data %>%
  ggplot(aes(y = extrapolation_estimate, x = year, fill = year)) +
  geom_col(show.legend = FALSE) +
  geom_pointrange(
    aes(ymin = extrapolation_estimate_lower, ymax = extrapolation_estimate_upper),
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      y = extrapolation_estimate_upper + 100000,
      label = round(extrapolation_estimate) %>% format(big.mark = ",")
    ),
    position = position_dodge()
  ) +
  geom_text(
    aes(
      y = 500000,
      label = round(colonies) %>%
        format(big.mark = ",") %>%
        paste0("Colonies:\n", .)
    ),
    position = position_dodge()
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::label_number_si()
  ) +
  scale_color_manual(
    values = colorBlindBlack8[c(2, 4, 6)],
    aesthetics = "fill", name = "Survey"
  ) +
  facet_wrap(~type) +
  labs(
    x = "",
    y = "Extrapolated Total Expenses [EUR] + 95% CI"
  ) +
  ggplot2::theme(
    panel.grid.major.y = element_line()
  )

fSaveImages(p, "extrapolation", h = 6)

# Estimate Extrapolation
p <- r_extrapolation$data_estimate %>%
  ggplot(aes(y = extrapolation_estimate, x = year, fill = year)) +
  geom_col(show.legend = FALSE) +
  geom_pointrange(
    aes(ymin = extrapolation_estimate_lower, ymax = extrapolation_estimate_upper),
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      y = extrapolation_estimate_upper + 100000,
      label = round(extrapolation_estimate) %>% format(big.mark = ",")
    ),
    position = position_dodge()
  ) +
  geom_text(
    aes(
      y = 500000,
      label = round(colonies) %>%
        format(big.mark = ",") %>%
        paste0("Colonies:\n", .)
    ),
    position = position_dodge()
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks(),
    labels = scales::label_number_si()
  ) +
  scale_color_manual(
    values = colorBlindBlack8[c(2, 4, 6)],
    aesthetics = "fill", name = "Survey"
  ) +
  facet_wrap(~type) +
  labs(
    x = "",
    y = "Extrapolated Estimated Expenses [EUR] + 95% CI"
  ) +
  ggplot2::theme(
    panel.grid.major.y = element_line()
  )

fSaveImages(p, "extrapolation-estimate", h = 6)


# difference
r_extrapolation$data %>%
  group_by(type) %>%
  summarise(mean(extrapolation_estimate)) %>%
  .[, 2] %>%
  unlist() %>%
  as.numeric() %>%
  diff()
