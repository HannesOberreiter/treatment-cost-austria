# Description -------------------------------------------------------------
# This file contains our treatment list ~ names of treatment methods,
# names will be used to connect costs, table column names etc.

# Generate List -----------------------------------------------------------
# tsingle, ttotal represent the col names from import
# tname, tshort is for plots and paper
# careful tname is also used in partials/standard-cost.R
treatmentList <- tibble(
  tsingle = c(
    "T_vcount_",
    "T_drone_",
    "T_hyperthermia_",
    "T_biotechnical_",
    "T_formic_short_",
    "T_formic_long_",
    "T_lactic_",
    "T_oxalic_trickle_pure_",
    "T_oxalic_vapo_",
    "T_oxalic_trickle_mix_",
    # "T_oxalic_trickle_",
    "T_thymol_",
    "T_synthetic_",
    "T_other_"
  ),
  ttotal = NA,
  tname = c(
    "Varroa monitoring",
    "Drone brood removal",
    "Hyperthermia",
    "Another biotechnical method",
    "Formic acid - short term",
    "Formic acid - long term",
    "Lactic acid",
    "Oxal acid - pure",
    "Oxal acid - sublimation",
    "Oxal acid - mixture",
    "Thymol",
    "Synthetic methods",
    "Another methods"
  ),
  tshort = c(
    "V-check",
    "Drone",
    "Hyp.",
    "Biot.",
    "Fa-ST",
    "Fa-LT",
    "Lactic",
    "Ox-pure",
    "Ox-sub",
    "Ox-mix",
    "Thy",
    "syn. Met.",
    "Another"
  )
) %>%
  mutate(
    ttotal = paste0(tsingle, "total")
  )