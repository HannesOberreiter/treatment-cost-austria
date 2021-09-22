# https://embed.tidymodels.org/articles/Applications/GLM.html
# https://www.tidymodels.org/start/case-study/
# https://www.lesahoffman.com/SPLH861/861_Lecture9_Generalized_Binary.pdf



library(tidymodels)
library(dotwhisker) # for visualizing regression results
library(embed)
# Fix the random numbers by setting the seed
# This enables the analysis to be reproducible when random numbers are used
set.seed(222)

# Only one entrie has a cost of zero, simply exclude it for the model
dfClean %>%
    filter(costs == 0) %>%
    nrow()

tmp <- dfClean %>%
    filter(costs != 0) %>%
    # Filter only rows which are complete
    filter(op_cert_org_beek %in% c("Ja", "Nein") & op_migratory_beekeeper %in% c("Ja", "Nein")) %>%
    mutate(
        # log2 for normal distribution
        costs = log2(costs),
        operation = forcats::as_factor(operation),
        t_short_od_lump = forcats::as_factor(t_short_od_lump),
        op_cert_org_beek = forcats::as_factor(op_cert_org_beek),
        op_migratory_beekeeper = forcats::as_factor(op_migratory_beekeeper)
    ) %>%
    mutate(
        across(ends_with("_yn"), as.factor)
    ) %>%
    select(-T_vcount_total_yn, -T_drone_total_yn) %>%
    select(id, costs, operation, op_cert_org_beek, op_migratory_beekeeper, ends_with("_yn"), hives_winter, t_desc_od)

# Put 3/4 of the data into the training set
data_split <- rsample::initial_split(tmp, prop = 3 / 4)
train_data <- rsample::training(data_split)
test_data <- rsample::testing(data_split)

data_rec <-
    recipe(costs ~ ., data = train_data) %>%
    update_role(id, new_role = "ID") %>%
    step_dummy(all_nominal_predictors()) %>%
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

data_fit <-
    data_wflow %>%
    fit(data = train_data)

data_fit %>%
    extract_fit_parsnip() %>%
    tidy()

predict(data_fit, test_data)
data_aug <-
    augment(data_fit, test_data)

predict(data_fit, train_data)
train_aug <-
    augment(data_fit, train_data)

glance(data_fit, test_data)
rmse(data_aug, costs, .pred)
rmse(train_data, costs, .pred)

# all treatments as dummies and rest all
# out of sample rmse 1.10
# train rmse
# AIC 7432,
# train
# AIC


data_aug %>%
    ggplot(aes(costs, .pred)) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::coord_equal() +
    ylim(c(0, NA)) +
    xlim(c(0, NA)) +
    geom_point()



library(performance)
m1 <- extract_model(r_model$fitted$fitted[[1]])
m2 <- extract_model(r_model$fitted$fitted[[2]])
m3 <- extract_model(r_model$best_model)

performance::check_collinearity(m) # looks good
performance::check_autocorrelation(m) # looks good
performance::check_heteroscedasticity(m) # http://www.statsmakemecry.com/smmctheblog/confusing-stats-terms-explained-heteroscedasticity-heteroske.html
performance::check_homogeneity(m, method = "auto") # looks good
performance::check_normality(m) # fails but test will most often resul tin non-normality
performance::check_outliers(m) # ok
performance::performance_accuracy(m) # we could report this
performance::test_performance(m1, m2, m3)
performance::compare_performance(m1, m2, m3)


# Vector of Countries we don't want
SADC_list <- c("Angola", "Botswana", "Comoros", "Democratic Republic of Congo", "Congo", "DRC", "Eswatini", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "Seychelles", "Tanzania", "Zambia", "Zimbabwe")
# Sample Data Frame
test_df <- data.frame(Country = c("Austria", "Angola", "Japan", "Vietnam", "DRC", "Lesotho", "Malawi"), x = runif(7))
# Base, using the %in% operator to return a logical vector if one Country is in the SADC_list
# We use the logical vector to subset our data.frame
!test_df$Country %in% SADC_list # logical vector as example, the ! in front will "negate", reverse the result
(new_df <- test_df[!(test_df$Country %in% SADC_list), ])

# Tidyverse Example, google tidyverse for more info
# library(tidyverse)
test_df %>%
    filter(!Country %in% SADC_list)



original <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6),
    c = c(7, 8, 9)
)

as.list(original)

apply(
    original, 1,
    function(rows) {
        data.frame(group = names(rows), x = rows) |>
        apply(
            1,
            function(x) {
                list(group = unname(x["group"]), x = unname(x["x"]))
            }
        ) |> unname()
    }
)

View(res)