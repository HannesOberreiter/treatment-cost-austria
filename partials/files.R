# Description -------------------------------------------------------------
# Main Logic to Import our Data
# Cleanup and Loss Rate Calculation
# Adding of Cols for Treatments etc.

# Dependent Files ---------------------------------------------------------
source("partials/standard-cost.R")

# Import ------------------------------------------------------------------
colTypes    <- rep("guess", 271)
# Alternative we could import everything as text?
# we need to define text here as our ID field is a little bit problematic
colTypes[3] <- "text" 
files       <- c("19-20", "18-19")
RAW         <- vector("list", length(files))
for(i in seq_along(files)){
  RAW[[i]] <- readxl::read_xlsx(
    str_c("data/", files[[i]], ".xlsx"),
    skip = 1, # First row is original names, we want to keep it in the xls file
    col_types = colTypes
    ) %>%
    add_column(year = str_replace(files[[i]], "-", "/"))
}
RAW <- bind_rows(RAW)
rm(i, files, colTypes)

# Mutating Columns --------------------------------------------------------

## ID ----------------------------------------------------------------------
RAW <- RAW %>% 
  mutate(
    id_original = id, # Backup our ID
    # Generate an Combination of year and id to prevent same IDs in both years
    id = str_c(id, year, sep = "-") 
  )

## Lost without Elements ---------------------------------------------------
# add hive lost without elements and spring hives for glm
# lost_a = queen losses; lost_b = elementar losses; lost_c = lost "normal"
RAW <- RAW %>% 
  mutate(
    hives_lost_e   = ifelse(is.na(lost_b), hives_lost, hives_lost - lost_b),
    hives_spring_e = hives_winter - hives_lost_e
  )

## Treatment Grouping ------------------------------------------------------
# Helper, change this do change months for grouping
seasons <- tibble(
  name    = c("spring", "summer", "winter"),
  rcol    = c("0[1-2]", "0[3-7]", "0[8-9]"),
  short   = c("SP", "SU", "WI"),
  desc    = c("SPRING", "SUMMER", "WINTER")
)

# Temporary for Combinations
RAW$c_short <- NA
RAW$c_desc  <- NA

for(i in seq_along(seasons$name)){
  #print(paste0("Season: ", seasons$name[i]))
  for(j in seq_along(treatmentList$tsingle)){
    #print(paste0("Treatment: ", treatmentList$tname[j]))
    treatmentexp <- paste0(
      "(", treatmentList$tsingle[j], ")\\S*", seasons$rcol[i] 
      )
    
    # Get Columns which are starting with List value
    x       <- grepl(treatmentexp, colnames(RAW), fixed = FALSE, perl = TRUE)
    rsums   <- rowSums(RAW[, x], na.rm = TRUE)
    
    # temporary colnames
    colname   <- paste(treatmentList$ttotal[j], seasons$name[i], sep = "_")
    colnameyn <- paste0(treatmentList$ttotal[j], "yn_", seasons$name[i])
    
    RAW <- RAW %>% add_column(
      # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
      !!(colname)   := rsums,
      # create a yes/no based on rsum
      !!(colnameyn) := ifelse(rsums > 0, 1, 0)
    )
    
    # Combination
    if(j == 1) next # dont add vcontrol to combination
    vlogical <- as.logical(rsums)
    colshort <- paste(seasons$short[i], treatmentList$tshort[j], sep="-")
    coldesc  <- paste(seasons$desc[i], treatmentList$tname[j], sep=" ")

    RAW$c_short[vlogical] <- paste(
      RAW$c_short[vlogical],
      colshort, 
      sep = " & "
    )
    RAW$c_desc[vlogical] <- paste(
      RAW$c_desc[vlogical],
      coldesc, 
      sep = " & "
    )

  }
}
# Cleanup
RAW$c_short <- stringr::str_replace(RAW$c_short, "NA &", "")
RAW$c_desc  <- stringr::str_replace(RAW$c_desc, "NA &", "")
rm(seasons, colname, colnameyn, i, j, rsums, treatmentexp, x, vlogical, colshort, coldesc)

## Treatment Total (no seasons) ------------------------------------------------------
# Temporary for Combinations, without seasons
RAW$t_short        <- NA
RAW$t_desc         <- NA
RAW$t_estimated    <- 0
RAW$t_short_number <- NA

# Loop through our Treatment Types
for(i in seq_along(treatmentList$tsingle)){
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
  x10    <- grepl(treatmentexp10, colnames(RAW), fixed = FALSE)
  rsum10 <- rowSums(RAW[, x10], na.rm = TRUE)
  x12    <- grepl(treatmentexp12, colnames(RAW), fixed = FALSE)
  rsum12 <- rowSums(RAW[, x12], na.rm = TRUE)
  
  colnameyn <- paste( treatmentList$ttotal[i], "_yn",  sep = "")
  
  RAW <- RAW %>% add_column(
    !!(treatmentList$ttotal[i])               := rsum10,
    !!(paste0(treatmentList$ttotal[i], "12")) := rsum12,
    # yes/no based on 10 months
    !!(colnameyn)                             := ifelse(rsum10 > 0, 1, 0)
  )
  
  # Combination & Expenses
  if(i == 1) next # dont add vcontrol to combination
  vlogical <- as.logical(rsum12)
  
  # Cost Estimated based on our Calculation
  estimated <- (
    (treatmentList$investment[i] / RAW$hives_winter) + # Investments are divided by the number of hives
       (treatmentList$material[i]) + # Material needs for each colony
          (treatmentList$consumables[i] * rsum12) # Consumables for each hive and each treatment (months in our case)
    ) * as.integer(vlogical) # we multiply with the logical, this means if it is zero all will be zero
  
  RAW$t_estimated <- RAW$t_estimated + estimated
  
  RAW$t_short[vlogical] <- paste(
    RAW$t_short[vlogical],
    treatmentList$tshort[i], 
    sep = " & "
  )
  RAW$t_desc[vlogical] <- paste(
    RAW$t_desc[vlogical],
    treatmentList$tname[i], 
    sep = " & "
  )
  
  if(i == 2) next # dont add dronebrood removal to number combination
  
  RAW$t_short_number[vlogical] <- paste(
    RAW$t_short_number[vlogical],
    paste(rsum12[vlogical], treatmentList$tshort[i], sep="-"), 
    sep = " & "
  )
  
}

# Cleanup
RAW$t_short <- stringr::str_replace(RAW$t_short, "NA &", "")
RAW$t_desc  <- stringr::str_replace(RAW$t_desc, "NA &", "")
RAW$t_short_number  <- stringr::str_replace(RAW$t_short_number, "NA &", "")
RAW$t_estimated <- round(RAW$t_estimated, 2)

rm(colnameyn, i, rsum10, rsum12, treatmentexp10, treatmentexp12, x10, x12, estimated, vlogical)

## Treatment Sums ----------------------------------------------------------
# sum rows for total different methods and SEASONS
# sum rows by yn_(season) column, that way we get amount of different treatments used
x <- grep("(yn_)", colnames(RAW), fixed = FALSE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_totalyn_)", colnames(RAW), fixed = FALSE)
x <- x[!(x %in% t)]
RAW$T_season <- rowSums(RAW[, x], na.rm = TRUE)
# sum rows by _yn column, that way we get amount of different treatments used
# if it 10 or 12 months defined by following line in previous loop
#     !!(colnameyn)                             := ifelse(rsum10 > 0, 1, 0)
x <- grep("_yn", colnames(RAW), fixed = TRUE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_total_yn)", colnames(RAW), fixed = FALSE)
x <- x[!(x %in% t)]
RAW$T_amount <- rowSums(RAW[, x], na.rm = TRUE)

rm(t,x)

# Clean User Input -------------------------------------------------------------
#### Overrule "wrong" User Input for varroa checked and treated question
RAW$varroa_checked[RAW$varroa_checked != "Ja" & RAW$T_vcount_total12 > 0] <- "Ja"
RAW$varroa_treated[RAW$varroa_treated != "Ja" & RAW$T_amount > 0]         <- "Ja"

# Mutate Cols without Drone Brood Removal ---------------------------------
RAW$t_desc_od  <- RAW$t_desc  %>% str_replace(., "Drone brood removal & ", "") %>% str_trim()
RAW$t_short_od <- RAW$t_short %>% str_replace(., "Drone & ", "") %>% str_trim()

RAW$c_desc_od  <- RAW$c_desc  %>% str_replace(., "SPRING Drone brood removal & ", "") %>% str_trim()
RAW$c_desc_od  <- RAW$c_desc_od  %>% str_replace(., "SUMMER Drone brood removal & ", "") %>% str_trim()
RAW$c_desc_od  <- RAW$c_desc_od  %>% str_replace(., "WINTER Drone brood removal & ", "") %>% str_trim()
RAW$c_short_od <- RAW$c_short %>% str_replace(., "SP-Drone & ", "") %>% str_trim()
RAW$c_short_od <- RAW$c_short_od %>% str_replace(., "SU-Drone & ", "") %>% str_trim()
RAW$c_short_od <- RAW$c_short_od %>% str_replace(., "WI-Drone & ", "") %>% str_trim()

# Lump Treatment < 10 ----------------------------------------------------------
RAW <- RAW %>% 
  group_by(year) %>%
  mutate(
    t_short_od_lump = forcats::fct_lump_min(t_short_od, 10),
    t_desc_od_lump  = forcats::fct_lump_min(t_desc_od, 10),
    c_short_od_lump = forcats::fct_lump_min(c_short_od, 10),
    c_desc_od_lump  = forcats::fct_lump_min(c_desc_od, 10)
  ) %>% ungroup()


# Remove Cols not needed --------------------------------------------------
RAW <- RAW %>% select(
  -starts_with(c("lost_", "symp_", "flow_")),
  -c("apiary_nearby", "weak", "young_queens", 
      "hives_spring_before", "queen_problems",
      "op_mash_bottom_board", "op_insulated_hives",
      "op_plastic_hives")
  ) %>% 
  select(-c(op_varroatolerant:crippled_bees)) %>% 
  select(-c(T_vcount_01:T_other_12))
