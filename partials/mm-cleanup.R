# Newpaper and Paper ####
RAW$submitted <- "Internet"
RAW$submitted[grepl("P", RAW$id_original, fixed = TRUE)] <- 'Paper'
RAW$submitted[grepl("Z", RAW$id_original, fixed = TRUE)] <- 'Newspaper'
COUNT_PAPER <- RAW %>% group_by(submitted) %>% summarise(n = n())

# Extract only answers with valid answer for costs
DATA <- RAW[!is.na(RAW$costs),]

# Extract No Treatment Answers ####
NO_TREATMENT <- DATA[DATA$varroa_treated != "Ja",]
NO_TREATMENT <- NO_TREATMENT[,c("id", "costs", "varroa_treated", "comments", "year", "T_amount", "c_short")]
DATA         <- DATA[!(DATA$id %in% NO_TREATMENT$id),]

# Extract Participants which did answer costs but did give no answer on what treatment ####
NO_METHOD <- DATA[is.na(DATA$t_short),]
NO_METHOD <- NO_METHOD[,c("id", "costs", "varroa_treated", "comments", "year", "T_amount", "c_short")]
DATA      <- DATA[!(DATA$id %in% NO_METHOD$id),]

# Entries with zero costs ####
COST_ZERO  <- DATA[DATA$costs == 0,]
COST_ZERO  <- COST_ZERO[,c("id", "id_original", "costs", "varroa_treated", "comments", "year", "T_amount", "c_short")]
# sponsorship (e.g. Imkereiförderung, Gemeinde)
id_sponsor <- c(1672, 2003, 2446, 1930)
# biotechnical & hyperthermie
id_keep    <- c(2534)
id_remove  <- COST_ZERO$id[!(COST_ZERO$id %in% id_keep)]
DATA       <- DATA[!(DATA$id %in% id_remove),]

# High Costs ####
csum <- summary(DATA$costs)
upper_limit <- csum[5] * 2
COST_UPPER_NO_ESTIMATE <- DATA[DATA$costs > upper_limit,]
COST_UPPER <- COST_UPPER_NO_ESTIMATE[COST_UPPER_NO_ESTIMATE$costs > COST_UPPER_NO_ESTIMATE$t_estimated*2,]
COST_UPPER <- COST_UPPER[,c("id", "id_original", "hives_winter", "hives_lost", "costs", "t_estimated", "varroa_treated", "comments", "year", "T_amount", "c_short")]
# Remove these entries, as they make sense
# 1976 explains that he bought a power generator and vaporizer
# 751 Bienensauna
id_nochange <- c(751, 1976)
COST_UPPER <- COST_UPPER %>% filter(!(id %in% id_nochange))
# Calculate new prices
COST_UPPER$new <- COST_UPPER$costs / COST_UPPER$hives_winter
DATA$costs[(DATA$id %in% COST_UPPER$id)] <- COST_UPPER$new
