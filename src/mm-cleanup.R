# Description -------------------------------------------------------------
# Cleanup of survey answers which must be wrong
# or not logical (eg. more than possible)
# also code snippets for reporting on material and methods section
mmList <- list()

## Motivation -------------------------------------------------------------
dfMotivation <- dfData %>%
  filter(year == "20/21" & submitted == "Internet") %>%
  select(id, starts_with("motivation_")) %>%
  pivot_longer(starts_with("motivation_")) %>%
  drop_na(value) %>%
  left_join(motivationList, by = c("name" = "cname")) %>%
  add_count(id, value) %>%
  glimpse()
# Check how many did not answer this question
mmList$motivation$no_answer <- dfMotivation %>%
  count(id, value) %>%
  filter(value == "Nein" & n == 21) %>%
  arrange(desc(n))
# Remove the entries without any answer
dfMotivation <- dfMotivation %>% filter(!(id %in% mmList$motivation$no_answer$id))
# Count for M&M text
mmList$motivation$valid_answers <- length(unique(dfMotivation$id))

## Paper and Newspaper -----------------------------------------------------
# The expenses question is only in the internet
# version available, therefore we need to report how much we drop
mmList$submitted_year <- dfData %>%
  group_by(submitted, year) %>%
  summarise(n = n())
mmList$submitted_all <- dfData %>%
  group_by(submitted) %>%
  summarise(n = n())

# Internet but did not answer costs
mmList$internet_no_answer <- dfData %>%
  filter(submitted == "Internet") %>%
  mutate(answered = is.na(costs)) %>%
  dplyr::add_count(year) %>%
  group_by(year, answered) %>%
  summarise(
    nn = n(),
    np = round(nn / first(n) * 100, 1)
  ) %>%
  filter(answered == TRUE)

# Generate DATA -----------------------------------------------------------
# Extract only answers with valid answer for costs
dfClean <- dfData[!is.na(dfData$costs), ]
dfClean %>% count(submitted)

## No Treatment Answer but Costs -----------------------------------------------------
mmList$no_treatment <- dfClean %>%
  filter(varroa_treated != "Ja") %>%
  select(c("id", "costs", "varroa_treated", "comments", "year", "t_amount", "c_short"))
dfClean <- dfClean %>% filter(!(id %in% mmList$no_treatment$id))

## No Treatment Method given -----------------------------------------------
# Extract Participants which did answer costs but
# did give no answer on what treatment
mmList$no_method <- dfClean %>%
  filter(is.na(t_short)) %>%
  select(c("id", "costs", "varroa_treated", "comments", "year", "t_amount", "c_short", "hives_winter"))
dfClean <- dfClean %>% filter(!(id %in% mmList$no_method$id))

## Zero Costs ------------------------------------------------------------
mmList$cost_zero <- list()
mmList$cost_zero$data <- dfClean %>%
  filter(costs == 0) %>%
  select(c("id", "costs", "varroa_treated", "comments", "year", "t_amount", "c_short"))
# sponsorship (e.g. Imkereiförderung, Gemeinde)
mmList$cost_zero$id_sponsor <- c("353-18/19", "752-18/19", "855-18/19", "1624-18/19")
# biotechnical & hyperthermie
mmList$cost_zero$id_keep <- c("1750-18/19")
mmList$cost_zero$id_remove <- mmList$cost_zero$data %>%
  filter(!(id %in% mmList$cost_zero$id_keep)) %>%
  pull(id)
dfClean <- dfClean %>%
  filter(!(id %in% mmList$cost_zero$id_remove))

## High Costs --------------------------------------------------------------
mmList$cost_upper <- list()
mmList$cost_upper$upper_limit <- (quantile(dfClean$costs, probs = 0.75, names = F) + 3 * IQR(dfClean$costs)) * 2
mmList$cost_upper$data <- dfClean %>%
  filter(costs >= mmList$cost_upper$upper_limit) %>%
  select(c("id", "varroa_treated", "comments", "year", "t_amount", "c_short", "costs", "t_estimated", "hives_winter")) %>%
  mutate(
    new_cost = round(costs / hives_winter)
  )

# Remove these entries, as they make sense
# Remove participants which used hyperthermia as we can make no assumptions about investement time
# 815-18/19 explains that he bought a power generator and vaporizer (not anymore inside our new limit, so he wont be changed anyway)
mmList$cost_upper$id_nochange <- c("815-18/19")
mmList$cost_upper$new_data <- mmList$cost_upper$data %>%
  filter(!(id %in% mmList$cost_upper$id_nochange | stringr::str_detect(c_short, "Hyp.")))
# add new calculated costs to our main df
dfClean$costs[(dfClean$id %in% mmList$cost_upper$new_data$id)] <- mmList$cost_upper$new_data$new_cost

# Outliers based on cohort
# tmpHelper <- dfClean %>%
#  group_by(t_short_od) %>%
#  summarise(
#    mean_costs = round(mean(costs)),
#    median_costs = median(costs),
#    q2 = quantile(costs, probs = 0.75, names = F) + 3 * IQR(costs)
#  ) %>%
#  glimpse()

# tmpDf <- dfClean %>%
#  left_join(tmpHelper, by = c("t_short_od")) %>%
#  filter(costs > q2) %>%
#  mutate(
#    new = round(costs / hives_winter)
#  ) %>%
#  select(c("answer: costs per unit" = "costs", "answer: total units" = "hives_winter", "extreme outliers breakpoint" = "q2", "mean_costs", "median_costs", "new: costs per unit after dividing units" = new))


## Difference --------------------------------------------------------------
mmList$reports <- dfData %>%
  count(year, name = "survey_n") %>%
  left_join(
    dfClean %>% count(year, name = "valid_n")
  ) %>%
  mutate(
    percent = round(valid_n / survey_n * 100)
  )
