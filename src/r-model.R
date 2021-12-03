# Description -------------------------------------------------------------
# Simple multiple regression linear model for evaluate influence and independence of our predictors
# For random forest and other stuff look at the playground folder, model.Rmd
library(tidymodels)
library(vip)
set.seed(222)

# Setup --------------------------
r_model <- list()
# Number of zero expenses answers
r_model$zero_expenses <- dfClean %>%
    filter(costs == 0) %>%
    nrow()

r_model$data <- dfClean %>%
    filter(costs != 0) %>%
    # because we log transform
    # filter(costs < 100) %>%
    filter(op_cert_org_beek %in% c("Ja", "Nein") & op_migratory_beekeeper %in% c("Ja", "Nein")) %>%
    mutate(
        op_cert_org_beek = forcats::as_factor(op_cert_org_beek),
        op_migratory_beekeeper = forcats::as_factor(op_migratory_beekeeper),
    ) %>%
    mutate(
        across(ends_with("_yn"), as.factor)
    ) %>%
    select(-T_vcount_total_yn, -T_drone_total_yn)

# Put 90% of the data into the training set
r_model$data_split <- rsample::initial_split(r_model$data, prop = 9 / 10, strata = c_short_od)
r_model$train_data <- rsample::training(r_model$data_split)
r_model$test_data <- rsample::testing(r_model$data_split)

# QQ Plots ------------------------------
# for M & M section to show how good the transformation is
p1 <- r_model$data %>%
    ggplot(aes(sample = log10(costs))) +
    geom_qq() +
    geom_qq_line() +
    ylab(TeX("Expenses/Colony \\[$\\log_{10}$ Euro\\]"))

p2 <- r_model$data %>%
    ggplot(aes(sample = log10(hives_winter))) +
    geom_qq() +
    geom_qq_line() +
    ylim(-2, 6.5) +
    ylab(TeX("Number of Colonies \\[$\\log_{10}$ #\\]"))

p <- (p1 | p2) +
    patchwork::plot_annotation(tag_levels = "A") &
    xlab("Theoretical Normal Distribution Quantiles") &
    coord_equal() &
    scale_y_continuous(
        breaks = c(-2, 0, 2, 4),
        limits = c(-2, 4)
    ) &
    scale_x_continuous(
        breaks = c(-4, -2, 0, 2, 4),
        limits = c(-4, 4)
    )

fSaveImages(p, "model-qq", w = 10, h = 4)

# Recipes ------------------------------
r_model$rec_intercept_only <-
    recipe(costs ~ 1, data = r_model$train_data) %>%
    step_log(costs, base = 10, skip = TRUE) %>%
    prep(training = r_model$train_data)

r_model$rec_base <-
    recipe(costs ~ hives_winter, data = r_model$train_data) %>%
    step_log(hives_winter, base = 10) %>%
    step_log(costs, base = 10, skip = TRUE) %>%
    prep(training = r_model$train_data)

r_model$rec_operational <-
    recipe(costs ~ hives_winter + op_cert_org_beek + op_migratory_beekeeper, data = r_model$train_data) %>%
    step_log(hives_winter, base = 10) %>%
    step_log(costs, base = 10, skip = TRUE) %>%
    prep(training = r_model$train_data)

r_model$rec_treatment <-
    recipe(costs ~ T_hyperthermia_total_yn + T_biotechnical_total_yn + T_formic_short_total_yn + T_formic_long_total_yn +
        T_lactic_total_yn + T_oxalic_trickle_pure_total_yn + T_oxalic_vapo_total_yn + T_oxalic_trickle_mix_total_yn +
        T_thymol_total_yn + T_synthetic_total_yn + T_other_total_yn + hives_winter + op_cert_org_beek + op_migratory_beekeeper, data = r_model$train_data) %>%
    step_log(hives_winter, base = 10) %>%
    step_log(costs, base = 10, skip = TRUE) %>%
    prep(training = r_model$train_data)

r_model$rec_state <-
    recipe(costs ~ T_hyperthermia_total_yn + T_biotechnical_total_yn + T_formic_short_total_yn + T_formic_long_total_yn +
        T_lactic_total_yn + T_oxalic_trickle_pure_total_yn + T_oxalic_vapo_total_yn + T_oxalic_trickle_mix_total_yn +
        T_thymol_total_yn + T_synthetic_total_yn + T_other_total_yn + hives_winter + op_cert_org_beek + op_migratory_beekeeper + state, data = r_model$train_data) %>%
    step_log(hives_winter, base = 10) %>%
    step_log(costs, base = 10, skip = TRUE) %>%
    prep(training = r_model$train_data)

# Models -----------------------------
r_model$lm_mod <-
    linear_reg() %>%
    set_engine("lm")

# Calculating ------------------------
r_model$data_wflows <-
    workflow_set(
        preproc = list(
            Intercept = r_model$rec_intercept_only,
            Base = r_model$rec_base,
            Operational = r_model$rec_operational,
            Treatment = r_model$rec_treatment,
            State = r_model$rec_state
        ),
        models = list(
            lm = r_model$lm_mod
        ),
        cross = FALSE
    )

r_model$fitted <- r_model$data_wflows %>%
    select(wflow_id, info) %>%
    unnest() %>%
    mutate(
        fitted = map(workflow, fit, data = r_model$train_data)
    ) %>%
    mutate(
        stat_parsnip = map(fitted, extract_fit_parsnip),
        stat_tidy = map(stat_parsnip, tidy),
        stat_values = map(fitted, glance)
    ) %>%
    unnest(stat_values)

# Manually selected best model based on AIC and BIC
# filter(AIC == min(AIC) & BIC == min(BIC))
(r_model$best_model <- r_model$fitted$fitted[[4]])

# Model Residuals Distribution ------------------------
p <- pull_workflow_fit(r_model$best_model)$fit$residuals %>%
    as_tibble() %>%
    ggplot(aes(sample = value)) +
    geom_qq() +
    geom_qq_line() +
    coord_equal() +
    labs(
        x = "Theoretical Quantiles (Normal Distribution)",
        y = "Training Dataset - Model Residuals Quantiles"
    )

fSaveImages(p, "model-qq-residuals", w = 8, h = 4)

# Performance Tests ------------------------
# library(performance)
# m <- extract_model(r_model$best_model)
## m1 <- extract_model(r_model$fitted$fitted[[1]])
## m2 <- extract_model(r_model$fitted$fitted[[2]])
## m3 <- extract_model(r_model$best_model)
# performance::check_collinearity(m) # looks good
# performance::check_autocorrelation(m) # looks good
# performance::check_heteroscedasticity(m) # http://www.statsmakemecry.com/smmctheblog/confusing-stats-terms-explained-heteroscedasticity-heteroske.html
# performance::check_homogeneity(m, method = "auto") # looks good
# performance::check_normality(m) # fails but test will most often result in non-normality
# performance::check_outliers(m) # ok
# performance::performance_accuracy(m) # we could report this
# performance::test_performance(m2, m3, denominator = m1)
# bayestestR::bayesfactor_models(m2, m3, denominator = m1)

# Vi Scores ------------------------

# Remove intercept only model otherwise we get an error
r_model$vi_scores <- r_model$fitted[2:5, ] %>%
    select(wflow_id, fitted) %>%
    mutate(
        vi_scores = map(
            fitted, ~ pull_workflow_fit(.x) %>%
                vi()
        )
    )

# pull_workflow_fit(r_model$best_model) %>%
#    vi()

# Coefficients ------------------------
r_model$coeff <- extract_fit_parsnip(r_model$best_model) %>%
    tidy() %>%
    mutate(
        name = str_remove_all(term, "_yn1"),
        estimate_conv = 10^abs(estimate) * ifelse(estimate < 0, -1, 1),
        error_conv = 10^std.error,
        direction = ifelse(estimate < 0, "Negative", "Positive"),
        direction = ifelse(name == "(Intercept)", "Neutral", direction)
    ) %>%
    left_join(treatmentList[2:3], by = c("name" = "ttotal")) %>%
    mutate(
        tname = case_when(
            name == "(Intercept)" ~ "(Intercept)",
            name == "hives_winter" ~ "Number of colonies",
            name == "op_cert_org_beekJa" ~ "Certified Organic",
            name == "op_migratory_beekeeperJa" ~ "Migratory Beekeeper",
            TRUE ~ paste0("T. ", tname)
        ),
        tname = forcats::fct_reorder(tname, estimate)
    )

r_model$coeff_intercept <- r_model$coeff %>%
    filter(name == "(Intercept)") %>%
    pull(estimate_conv, error_conv)

p <- r_model$coeff %>%
    filter(name != "(Intercept)") %>%
    ggplot(aes(x = estimate_conv, y = tname, color = direction)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange(
        aes(
            xmin = estimate_conv - error_conv,
            xmax = estimate_conv + error_conv
        ),
        show.legend = FALSE
    ) +
    scale_x_continuous(
        breaks = scales::pretty_breaks()
    ) +
    scale_colour_manual(
        values = c("#D55E00", "#009E73")
    ) +
    xlab("Estimate + Standard Error [Euro]") +
    ylab("Coefficients") +
    theme(
        panel.grid.major.y = element_line(),
        panel.grid.major.x = element_line(),
        axis.text.y = element_text(hjust = 0)
    )

fSaveImages(p, "model-whisker", w = 8, h = 5)

# Other Model Params Extractions ------------------------
r_model$pvalues <- r_model$fitted %>%
    select(wflow_id, fitted) %>%
    mutate(
        pvalue =
            map(
                fitted, ~ extract_fit_parsnip(.x) %>%
                    tidy() %>%
                    arrange(p.value)
            )
    ) %>%
    unnest(pvalue)

r_model$prediction <-
    r_model$fitted %>%
    select(wflow_id, fitted) %>%
    mutate(
        prediction_test = map(fitted, ~ tibble(.fitted = predict(.x, r_model$test_data, type = "raw"), costs = r_model$test_data$costs) %>%
            mutate(
                type = "Testing",
                costs = log10(costs)
            )),
        prediction_train = map(fitted, ~ augment(extract_model(.x), r_model$train_data) %>%
            select(costs, .fitted) %>%
            mutate(
                type = "Training",
                costs = log10(costs)
            ))
    )

r_model$performance_accuracy <- r_model$fitted[2:5, ] %>%
    select(wflow_id, fitted) %>%
    mutate(
        accuracy = map(
            fitted, ~ extract_model(.x) %>%
                performance::performance_accuracy()
        )
    )

r_model$performance_accuracy_best <- r_model$performance_accuracy %>%
    filter(wflow_id == "Treatment_lm") %>%
    pull(accuracy) %>%
    pluck(1)

r_model$prediction_stats <- bind_rows(
    r_model$prediction %>% filter(wflow_id == "Treatment_lm") %>% unnest(prediction_test) %>%
        yardstick::rmse(., costs, .fitted) %>% mutate(type = "Training"),
    r_model$prediction %>% filter(wflow_id == "Treatment_lm") %>% unnest(prediction_train) %>%
        yardstick::rmse(., costs, .fitted) %>% mutate(type = "Testing"),
)

p <- bind_rows(r_model$prediction %>% unnest(prediction_test), r_model$prediction %>% unnest(prediction_train)) %>%
    filter(wflow_id %in% c("Intercept_lm", "Treatment_lm")) %>%
    mutate(
        wflow_id = ifelse(wflow_id == "Intercept_lm", "Intercept Only", "Best Model"),
        wflow_id = forcats::fct_relevel(wflow_id, "Best Model", after = 1)
    ) %>%
    left_join(r_model$prediction_stats) %>%
    mutate(label = glue("{type} (RMSE: {round(.estimate,2)})")) %>%
    ggplot(aes(costs, .fitted, color = wflow_id, group = wflow_id)) +
    ylab(TeX("Prediction \\[$\\log_{10}$ Euro\\]")) +
    xlab(TeX("Survey \\[$\\log_{10}$ Euro\\]")) +
    geom_point(alpha = .15, show.legend = FALSE, color = "black") +
    geom_smooth(method = "lm") +
    geom_abline(color = colorBlindBlack8[8]) +
    labs(color = "") +
    ggplot2::scale_color_manual(values = colorBlindBlack8[2:3]) +
    coord_obs_pred() +
    facet_wrap(~label) +
    # https://stackoverflow.com/questions/21066077/remove-fill-around-legend-key-in-ggplot
    guides(color = guide_legend(override.aes = list(fill = NA))) +
    ggplot2::theme(
        legend.position = "top"
    )
fSaveImages(p, "model-pred", w = 8, h = 5)

saveRDS(r_model, "output/r-model.rds")