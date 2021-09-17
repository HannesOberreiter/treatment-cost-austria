# Description -------------------------------------------------------------
# Simple multiple regression linear model for evulation independence of our factors
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
    filter(op_cert_org_beek %in% c("Ja", "Nein") & op_migratory_beekeeper %in% c("Ja", "Nein")) %>%
    mutate(
        # log for somewhat normal distribution
        costs = log(costs),
        op_cert_org_beek = forcats::as_factor(op_cert_org_beek),
        op_migratory_beekeeper = forcats::as_factor(op_migratory_beekeeper),
    ) %>%
    mutate(
        across(ends_with("_yn"), as.factor)
    ) %>%
    select(-T_vcount_total_yn, -T_drone_total_yn)

# Put 3/4 of the data into the training set
r_model$data_split <- rsample::initial_split(r_model$data, prop = 3 / 4)
r_model$train_data <- rsample::training(r_model$data_split)
r_model$test_data <- rsample::testing(r_model$data_split)

# Recipes ------------------------------
r_model$rec_base <-
    recipe(costs ~ hives_winter, data = r_model$train_data) %>%
    step_log(hives_winter, base = 10) %>%
    prep(training = r_model$train_data)

r_model$rec_operational <-
    recipe(costs ~ hives_winter + op_cert_org_beek + op_migratory_beekeeper, data = r_model$train_data) %>%
    step_log(hives_winter, base = 10) %>%
    prep(training = r_model$train_data)

r_model$rec_treatment <-
    recipe(costs ~ T_hyperthermia_total_yn + T_biotechnical_total_yn + T_formic_short_total_yn + T_formic_long_total_yn +
        T_lactic_total_yn + T_oxalic_trickle_pure_total_yn + T_oxalic_vapo_total_yn + T_oxalic_trickle_mix_total_yn +
        T_thymol_total_yn + T_synthetic_total_yn + T_other_total_yn + hives_winter + op_cert_org_beek + op_migratory_beekeeper, data = r_model$train_data) %>%
    step_log(hives_winter, base = 10) %>%
    prep(training = r_model$train_data)

# Models -----------------------------
r_model$lm_mod <-
    linear_reg() %>%
    set_engine("lm")

# Calculating ------------------------
r_model$data_wflows <-
    workflow_set(
        preproc = list(
            Base = r_model$rec_base,
            Operational = r_model$rec_operational,
            Treatment = r_model$rec_treatment
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
    glimpse()


r_model$fitted <- r_model$fitted %>%
    mutate(
        stat_parsnip = map(fitted, extract_fit_parsnip),
        stat_tidy = map(stat_parsnip, tidy),
        stat_values = map(fitted, glance)
    ) %>%
    unnest(stat_values)

r_model$best_model <- r_model$fitted$fitted[[3]]

r_model$vi_scores <- r_model$best_model %>%
    pull_workflow_fit() %>%
    vi()

r_model$pvalues <- r_model$best_model %>%
    extract_fit_parsnip() %>%
    tidy() %>%
    arrange(p.value)

r_model$prediction_test <-
    augment(r_model$best_model, r_model$test_data) %>%
    select(costs, .pred) %>%
    mutate(type = "Testing")

r_model$prediction_train <-
    augment(r_model$best_model, r_model$train_data) %>%
    select(costs, .pred) %>%
    mutate(type = "Training")

r_model$prediction_stats <- bind_rows(
    yardstick::rmse(r_model$prediction_train, costs, .pred) %>% mutate(type = "Training"),
    yardstick::rmse(r_model$prediction_test, costs, .pred) %>% mutate(type = "Test"),
)

p <- bind_rows(r_model$prediction_test, r_model$prediction_train) %>%
    ggplot(aes(costs, .pred)) +
    ylab("Prediction [log(Euro)]") +
    xlab("Survey [log(Euro)]") +
    geom_point(alpha = .15) +
    geom_smooth(method = "lm", color = colorBlindBlack8[4]) +
    geom_abline(color = colorBlindBlack8[8]) +
    coord_obs_pred() +
    facet_wrap(~type)

fSaveImages(p, "model-pred", w = 8, h = 5)
