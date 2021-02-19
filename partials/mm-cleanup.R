# Description -------------------------------------------------------------
# Cleanup of survey answers which must be wrong
# or not logical (eg. more than possible)
# also code snippets for reporting on material and methods section

# Paper and Newspaper -----------------------------------------------------
# The expenses question is only in the internet 
# version available, therefore we need to report how much we drop
RAW$submitted <- "Internet"
RAW$submitted[grepl("P", RAW$id_original, fixed = TRUE)] <- 'Paper'
RAW$submitted[grepl("Z", RAW$id_original, fixed = TRUE)] <- 'Newspaper'
SUMMARY_submitted <- RAW %>% 
  group_by(submitted, year) %>% summarise(n = n())

# Generate DATA -----------------------------------------------------------
# Extract only answers with valid answer for costs
DATA <- RAW[!is.na(RAW$costs),]
DATA %>% count(submitted)

## No Treatment Answer but Costs -----------------------------------------------------
NO_TREATMENT <- DATA %>% filter(varroa_treated != "Ja")
NO_TREATMENT <- NO_TREATMENT %>% 
  select(c("id", "costs", "varroa_treated", "comments", "year", "T_amount", "c_short"))
DATA         <- DATA %>% filter(!(id %in% NO_TREATMENT$id))

## No Treatment Method given -----------------------------------------------
# Extract Participants which did answer costs but 
# did give no answer on what treatment
NO_METHOD <- DATA %>% filter(is.na(t_short))
NO_METHOD <- NO_METHOD %>% 
  select(c("id", "costs", "varroa_treated", "comments", "year", "T_amount", "c_short"))
DATA      <- DATA %>% filter(!(id %in% NO_METHOD$id))

## Zero Costs ------------------------------------------------------------
NO_COST_ZERO            <- list()
NO_COST_ZERO[["data"]]  <- DATA %>% filter(costs == 0)
NO_COST_ZERO[["data"]]  <- NO_COST_ZERO[["data"]] %>% 
  select(c("id", "costs", "varroa_treated", "comments", "year", "T_amount", "c_short"))
# sponsorship (e.g. Imkereiförderung, Gemeinde)
NO_COST_ZERO[["id_sponsor"]] <- c("353-18/19", "752-18/19", "855-18/19", "1624-18/19")
# biotechnical & hyperthermie
NO_COST_ZERO[["id_keep"]]    <- c("1750-18/19")
NO_COST_ZERO[["id_remove"]]  <- NO_COST_ZERO[["data"]] %>% 
  filter(!(id %in% NO_COST_ZERO[["id_keep"]]))
DATA                         <- DATA %>% 
  filter(!(id %in% NO_COST_ZERO[["id_remove"]]$id))

## High Costs --------------------------------------------------------------
NO_COST_UPPER <- list()
# upper limit two times the 3th (75%) quantile
NO_COST_UPPER[["upper_limit"]] <- quantile(DATA$costs, probs = 0.75, names = F) * 2
NO_COST_UPPER[["COST_UPPER_NO_ESTIMATE"]] <- DATA %>% 
  filter(costs > NO_COST_UPPER[["upper_limit"]]) %>% 
  select(c("id", "costs", "varroa_treated", "comments", "year", "T_amount", "c_short", "t_estimated", "hives_winter"))
NO_COST_UPPER[["COST_UPPER"]] <- NO_COST_UPPER[["COST_UPPER_NO_ESTIMATE"]]  %>% 
  filter(costs > t_estimated * 2)
# Remove these entries, as they make sense
# 815-18/19 explains that he bought a power generator and vaporizer
# 3959-19/20 Bienensauna
NO_COST_UPPER[["id_nochange"]] <- c("3959-19/20", "815-18/19")
NO_COST_UPPER[["NEW_COST"]]   <- NO_COST_UPPER[["COST_UPPER"]] %>% 
  filter(!(id %in% NO_COST_UPPER[["id_nochange"]])) %>% 
  mutate(
    # Calculate new prices, as we think that they answered a total costs for all colonies
    new_cost = costs / hives_winter
  )
# add new calculated costs to our main df
DATA$costs[(DATA$id %in% NO_COST_UPPER[["NEW_COST"]]$id)] <- NO_COST_UPPER[["NEW_COST"]]$new_cost
