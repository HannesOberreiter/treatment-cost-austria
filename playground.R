# https://embed.tidymodels.org/articles/Applications/GLM.html
# https://www.tidymodels.org/start/case-study/
# https://www.lesahoffman.com/SPLH861/861_Lecture9_Generalized_Binary.pdf

# TODO https://www.tidymodels.org/learn/statistics/infer/
# Change Coin Package to simple resampling p-value reporting?

library(tidymodels)
library(dotwhisker) # for visualizing regression results
library(embed)


# Fix the random numbers by setting the seed
# This enables the analysis to be reproducible when random numbers are used
set.seed(222)

tmp <- dfClean %>%
    filter(op_cert_org_beek %in% c("Ja", "Nein") & op_migratory_beekeeper %in% c("Ja", "Nein")) %>%
    mutate(
        costs = log2(costs),
        operation = forcats::as_factor(operation),
        t_short_od_lump = forcats::as_factor(t_short_od_lump),
        op_cert_org_beek = forcats::as_factor(op_cert_org_beek),
        op_migratory_beekeeper = forcats::as_factor(op_migratory_beekeeper)
    ) %>%
    select(id, costs, operation, op_cert_org_beek, op_migratory_beekeeper, ends_with("_yn"))

# Put 3/4 of the data into the training set
data_split <- rsample::initial_split(tmp, prop = 0.9)
# Create data frames for the two sets:
train_data <- rsample::training(data_split)
test_data <- rsample::testing(data_split)

data_rec <-
    recipe(costs ~ ., data = train_data) %>%
    update_role(id, new_role = "ID") %>%
    step_dummy(all_nominal_predictors()) %>%
    step_lencode_glm(costs, outcome = vars(costs)) %>%
    # estimate the effects
    prep(training = train_data)

data_rec

summary(data_rec)
lr_mod <-
    linear_reg() %>%
    set_engine("lm")



data_wflow <-
    workflow() %>%
    add_model(lr_mod) %>%
    add_recipe(data_rec)

data_wflow

data_fit <-
    data_wflow %>%
    fit(data = train_data)

data_fit %>%
    extract_fit_parsnip() %>%
    tidy()


predict(data_fit, test_data)

data_aug <-
    augment(data_fit, test_data)

glance(data_fit, test_data)

rmse(data_aug, costs, .pred)

data_aug %>%
    ggplot(aes(costs, .pred)) +
    ggplot2::coord_equal() +
    ylim(c(0, NA)) +
    xlim(c(0, NA)) +
    geom_point()




