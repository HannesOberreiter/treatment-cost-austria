# Description -------------------------------------------------------------
# Main Logic to Import our Data
# Cleanup and Loss Rate Calculation
# Adding of Cols for Treatments etc.

# Dependent Files ---------------------------------------------------------

# Load Data --------------------------------------------------------------
files <- c("19_20", "18_19", "20_21")
dfData <- vector("list", length(files))

for (i in seq_along(files)) {
  dfData[[i]] <- readxl::read_xlsx(
    str_c(here(), "/data/", files[[i]], ".xlsx"),
    skip = 1, # First row is original names, we want to keep it in the xlsx file
    col_types = "text"
  ) %>%
    add_column(year = str_replace(files[[i]], "_", "/"))
}
dfData <- bind_rows(dfData)
dfData <- dfData %>% select(-starts_with("...")) # Drop empty cols
rm(i, files)

# Mutating Columns --------------------------------------------------------

## Set Numeric Cols --------------------------------------------------------
dfData <- dfData %>%
  mutate(
    across(altitude:longitude, as.numeric),
    across(apiaries, as.numeric),
    across(hives_winter:young_queens, as.numeric),
    across(T_vcount_01:T_other_12, as.numeric),
    time = as.numeric(time),
    costs = as.numeric(costs),
    loss_acceptance = as.numeric(loss_acceptance)
  )


## ID ----------------------------------------------------------------------
dfData <- dfData %>%
  mutate(
    id_original = id, # Backup our ID
    # Generate an Combination of year and id to prevent same IDs in both years
    id = str_c(id, year, sep = "-")
  )

# Clean NAs ---------------------------------------------------------------
# Ifelse question to prevent NA Errors and wrong numbers
dfData$hives_winter[is.na(dfData$hives_winter)] <- 0
dfData$hives_lost[is.na(dfData$hives_lost)] <- 0
dfData$lost_a[is.na(dfData$lost_a)] <- 0 # Lost - Queen
dfData$lost_b[is.na(dfData$lost_b)] <- 0 # Lost - Elements
dfData$lost_c[is.na(dfData$lost_c)] <- 0 # Lost - Other
# Symptoms clean NAs
dfData$symp_a[is.na(dfData$symp_a)] <- 0
dfData$symp_b[is.na(dfData$symp_b)] <- 0
dfData$symp_c[is.na(dfData$symp_c)] <- 0
dfData$symp_d[is.na(dfData$symp_d)] <- 0
dfData$symp_e[is.na(dfData$symp_e)] <- 0

## Lost without Elements ---------------------------------------------------
# add hive lost without elements and spring hives for glm
# lost_a = queen losses; lost_b = elementar losses; lost_c = lost "normal"
dfData <- dfData %>%
  mutate(
    hives_lost_e = hives_lost - lost_b,
    hives_spring_e = hives_winter - hives_lost_e,
    hives_spring_queen = hives_winter - lost_a, # Values for Queens
    lost_rate = hives_lost / hives_winter * 100, # Loss rate per company %
    lost_rate_e = hives_lost_e / hives_winter * 100,
    hives_per_apiary = hives_winter / apiaries # hives per apiary
  )

## Submitting Type ---------------------------------------------------------
dfData$submitted <- "Internet"
dfData$submitted[grepl("P", dfData$id, fixed = TRUE)] <- "Paper"
dfData$submitted[grepl("Z", dfData$id, fixed = TRUE)] <- "Newspaper"

## Treatment Grouping ------------------------------------------------------
# Helper, change this do change months for grouping
seasons <- tibble(
  name = c("spring", "summer", "winter"),
  rcol = c("0[1-2]", "0[3-7]", "0[8-9]"),
  short = c("SP", "SU", "WI"),
  desc = c("SPRING", "SUMMER", "WINTER")
)

# Temporary for Combinations
dfData$c_short <- NA
dfData$c_desc <- NA

for (i in seq_along(seasons$name)) {
  # print(paste0("Season: ", seasons$name[i]))
  for (j in seq_along(treatmentList$tsingle)) {
    # print(paste0("Treatment: ", treatmentList$tname[j]))
    treatmentexp <- paste0(
      "(", treatmentList$tsingle[j], ")\\S*", seasons$rcol[i]
    )

    # Get Columns which are starting with List value
    x <- grepl(treatmentexp, colnames(dfData), fixed = FALSE, perl = TRUE)
    rsums <- rowSums(dfData[, x], na.rm = TRUE)

    # temporary colnames
    colname <- paste(treatmentList$ttotal[j], seasons$name[i], sep = "_")
    colnameyn <- paste0(treatmentList$ttotal[j], "yn_", seasons$name[i])

    dfData <- dfData %>% add_column(
      # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
      !!(colname) := rsums,
      # create a yes/no based on rsum
      !!(colnameyn) := ifelse(rsums > 0, 1, 0)
    )

    # Combination
    if (j == 1) next # dont add vcontrol to combination
    vlogical <- as.logical(rsums)
    colshort <- paste(seasons$short[i], treatmentList$tshort[j], sep = "-")
    coldesc <- paste(seasons$desc[i], treatmentList$tname[j], sep = " ")

    dfData$c_short[vlogical] <- paste(
      dfData$c_short[vlogical],
      colshort,
      sep = " & "
    )
    dfData$c_desc[vlogical] <- paste(
      dfData$c_desc[vlogical],
      coldesc,
      sep = " & "
    )
  }
}
# Cleanup
dfData$c_short <- stringr::str_replace(dfData$c_short, "NA &", "")
dfData$c_desc <- stringr::str_replace(dfData$c_desc, "NA &", "")
rm(seasons, colname, colnameyn, i, j, rsums, treatmentexp, x, vlogical, colshort, coldesc)

## Treatment Total (no seasons) ------------------------------------------------------
# Temporary for Combinations, without seasons
dfData$t_short <- NA
dfData$t_desc <- NA
dfData$t_estimated <- 0
dfData$t_short_number <- NA

# Loop through our Treatment Types
for (i in seq_along(treatmentList$tsingle)) {
  # Get Columns which are starting with List value
  # double blackslash otherwise R wont escape the backslash
  # WE USE HERE all 12 months for spring, summer, winter we only use 01-10 months!
  treatmentexp10 <- paste0(
    "(", treatmentList$tsingle[i], ")\\S*0[1-9]|(", treatmentList$tsingle[i], ")\\S*1[0]"
  )
  treatmentexp12 <- paste0(
    "(", treatmentList$tsingle[i], ")\\S*0[1-9]|(", treatmentList$tsingle[i], ")\\S*1[0-2]"
  )
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  x10 <- grepl(treatmentexp10, colnames(dfData), fixed = FALSE)
  rsum10 <- rowSums(dfData[, x10], na.rm = TRUE)
  x12 <- grepl(treatmentexp12, colnames(dfData), fixed = FALSE)
  rsum12 <- rowSums(dfData[, x12], na.rm = TRUE)

  colnameyn <- paste(treatmentList$ttotal[i], "_yn", sep = "")

  dfData <- dfData %>% add_column(
    !!(treatmentList$ttotal[i]) := rsum10,
    !!(paste0(treatmentList$ttotal[i], "12")) := rsum12,
    # yes/no based on 10 months
    !!(colnameyn) := ifelse(rsum10 > 0, 1, 0)
  )

  # Combination & Expenses
  if (i == 1) next # dont add vcontrol to combination
  vlogical <- as.logical(rsum12)

  # Cost Estimated based on our Calculation
  estimated <- (
    (treatmentList$investment[i] / dfData$hives_winter) + # Investments are divided by the number of hives
      (treatmentList$material[i]) + # Material needs for each colony
      (treatmentList$consumables[i] * rsum12) # Consumables for each hive and each treatment (months in our case)
  ) * as.integer(vlogical) # we multiply with the logical, this means if it is zero all will be zero

  dfData$t_estimated <- dfData$t_estimated + estimated

  dfData$t_short[vlogical] <- paste(
    dfData$t_short[vlogical],
    treatmentList$tshort[i],
    sep = " & "
  )
  dfData$t_desc[vlogical] <- paste(
    dfData$t_desc[vlogical],
    treatmentList$tname[i],
    sep = " & "
  )

  if (i == 2) next # dont add dronebrood removal to number combination

  dfData$t_short_number[vlogical] <- paste(
    dfData$t_short_number[vlogical],
    paste(rsum12[vlogical], treatmentList$tshort[i], sep = "-"),
    sep = " & "
  )
}

# Cleanup
dfData$t_short <- stringr::str_replace(dfData$t_short, "NA &", "")
dfData$t_desc <- stringr::str_replace(dfData$t_desc, "NA &", "")
dfData$t_short_number <- stringr::str_replace(dfData$t_short_number, "NA &", "")
dfData$t_estimated <- round(dfData$t_estimated, 2)

rm(colnameyn, i, rsum10, rsum12, treatmentexp10, treatmentexp12, x10, x12, estimated, vlogical)

## Treatment Sums ----------------------------------------------------------
# sum rows for total different methods and SEASONS
# sum rows by yn_(season) column, that way we get amount of different treatments used
x <- grep("(yn_)", colnames(dfData), fixed = FALSE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_totalyn_)", colnames(dfData), fixed = FALSE)
x <- x[!(x %in% t)]
dfData$t_season <- rowSums(dfData[, x], na.rm = TRUE)
# sum rows by _yn column, that way we get amount of different treatments used
# if it 10 or 12 months defined by following line in previous loop
#     !!(colnameyn)                             := ifelse(rsum10 > 0, 1, 0)
x <- grep("_yn", colnames(dfData), fixed = TRUE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_total_yn)", colnames(dfData), fixed = FALSE)
x <- x[!(x %in% t)]
dfData$t_amount <- rowSums(dfData[, x], na.rm = TRUE)

rm(t, x)

# Clean User Input -------------------------------------------------------------
#### Overrule "wrong" User Input for varroa checked and treated question
dfData$varroa_checked[dfData$varroa_checked != "Ja" & dfData$T_vcount_total12 > 0] <- "Ja"
dfData$varroa_treated[dfData$varroa_treated != "Ja" & dfData$t_amount > 0] <- "Ja"

# Mutate Cols without Drone Brood Removal ---------------------------------
dfData$t_desc_od <- dfData$t_desc %>%
  str_replace(., glue::glue("{treatmentList$tname[[2]]} & "), "") %>%
  str_trim()
dfData$t_short_od <- dfData$t_short %>%
  str_replace(., glue::glue("{treatmentList$tshort[[2]]} & "), "") %>%
  str_trim()
dfData$c_desc_od <- dfData$c_desc %>%
  str_replace(., glue::glue("Frühjahr {treatmentList$tname[[2]]} & "), "") %>%
  str_trim()
dfData$c_desc_od <- dfData$c_desc_od %>%
  str_replace(., glue::glue("Sommer {treatmentList$tname[[2]]} & "), "") %>%
  str_trim()
dfData$c_desc_od <- dfData$c_desc_od %>%
  str_replace(., glue::glue("Winter {treatmentList$tname[[2]]} & "), "") %>%
  str_trim()
dfData$c_short_od <- dfData$c_short %>%
  str_replace(., glue::glue("F-{treatmentList$tshort[[2]]} & "), "") %>%
  str_trim()
dfData$c_short_od <- dfData$c_short_od %>%
  str_replace(., glue::glue("S-{treatmentList$tshort[[2]]} & "), "") %>%
  str_trim()
dfData$c_short_od <- dfData$c_short_od %>%
  str_replace(., glue::glue("W-{treatmentList$tshort[[2]]} & "), "") %>%
  str_trim()

# Lump Treatment < 10 ----------------------------------------------------------
dfData <- dfData %>%
  group_by(year) %>%
  mutate(
    t_short_od_lump = forcats::fct_lump_min(t_short_od, 10),
    t_desc_od_lump = forcats::fct_lump_min(t_desc_od, 10),
    c_short_od_lump = forcats::fct_lump_min(c_short_od, 10),
    c_desc_od_lump = forcats::fct_lump_min(c_desc_od, 10)
  ) %>%
  ungroup()

# Global Helper for GLM ---------------------------------------------------
dfData <- dfData %>%
  mutate(
    global = as.factor(1)
  )

# Remove Cols not needed --------------------------------------------------
dfData <- dfData %>%
  select(
    -starts_with(c("lost_", "symp_", "flow_")),
    -c(
      "apiary_nearby", "weak", "young_queens",
      "hives_spring_before", "queen_problems",
      "op_mash_bottom_board", "op_insulated_hives",
      "op_plastic_hives"
    )
  ) %>%
  select(-c(op_varroatolerant:crippled_bees)) %>%
  select(-c(T_vcount_01:T_other_12))