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
r_model$rec_intercept_only <-
    recipe(costs ~ 1, data = r_model$train_data) %>%
    # step_log(hives_winter, base = 10) %>%
    prep(training = r_model$train_data)

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
            Intercept = r_model$rec_intercept_only,
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

# Remove intercept only model otherwise we get an error
r_model$vi_scores <- r_model$fitted[2:4, ] %>%
    select(wflow_id, fitted) %>%
    mutate(
        vi_scores = map(
            fitted, ~ pull_workflow_fit(.x) %>%
                vi()
        )
    )


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
        prediction_test = map(fitted, ~ augment(.x, r_model$test_data) %>%
            select(costs, .pred) %>%
            mutate(type = "Testing")),
        prediction_train = map(fitted, ~ augment(.x, r_model$train_data) %>%
            select(costs, .pred) %>%
            mutate(type = "Training"))
    )

r_model$performance_accuracy <- r_model$fitted[2:4, ] %>%
    select(wflow_id, fitted) %>%
    mutate(
        accuracy = map(
            fitted, ~ extract_model(.x) %>%
                performance::performance_accuracy()
        )
    )

# r_model$performance_accuracy$accuracy[[1]]["Accuracy"]

r_model$prediction_stats <- bind_rows(
    r_model$prediction %>% filter(wflow_id == "Treatment_lm") %>% unnest(prediction_test) %>%
        yardstick::rmse(., costs, .pred) %>% mutate(type = "Training"),
    r_model$prediction %>% filter(wflow_id == "Treatment_lm") %>% unnest(prediction_train) %>%
        yardstick::rmse(., costs, .pred) %>% mutate(type = "Test"),
)

p <- bind_rows(r_model$prediction %>% unnest(prediction_test), r_model$prediction %>% unnest(prediction_train)) %>%
    filter(wflow_id %in% c("Intercept_lm", "Treatment_lm")) %>%
    mutate(wflow_id = ifelse(wflow_id == "Intercept_lm", "Intercept Only", "Best Model")) %>%
    ggplot(aes(costs, .pred, color = wflow_id, group = wflow_id)) +
    ylab("Prediction [log(Euro)]") +
    xlab("Survey [log(Euro)]") +
    geom_point(alpha = .15, show.legend = FALSE) +
    geom_smooth(method = "lm") +
    geom_abline(color = colorBlindBlack8[8]) +
    labs(color = "Model") +
    ggplot2::scale_color_manual(values = colorBlindBlack8[1:2]) +
    coord_obs_pred() +
    facet_wrap(~type) +
    ggplot2::theme(
        legend.position = "top"
    )

fSaveImages(p, "model-pred", w = 8, h = 5)

# Should report some perfomrance tests?
# library(performance)
# m1 <- extract_model(r_model$fitted$fitted[[1]])
# m2 <- extract_model(r_model$fitted$fitted[[2]])
# m3 <- extract_model(r_model$best_model)
#
# performance::check_collinearity(m) # looks good
# performance::check_autocorrelation(m) # looks good
# performance::check_heteroscedasticity(m) # http://www.statsmakemecry.com/smmctheblog/confusing-stats-terms-explained-heteroscedasticity-heteroske.html
# performance::check_homogeneity(m, method = "auto") # looks good
# performance::check_normality(m) # fails but test will most often resul tin non-normality
# performance::check_outliers(m) # ok
# performance::performance_accuracy(m) # we could report this
# performance::test_performance(m1, m2, m3)
# performance::compare_performance(m1, m2, m3)