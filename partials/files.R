# Source our prices
source("partials/standard-cost.R")

# Importing our files, we have two survey years
# first row is original names, we want to keep it in the xls file
col_types = rep("guess", 271) 
col_types[3] = "text" # we need to define text here as our ID field is a little bit problematic
D1920 <- tibble::as_tibble(
  readxl::read_xlsx("data/19-20.xlsx", skip=1, col_types = col_types)
  ) %>% add_column(year = "19/20")
D1819 <- tibble::as_tibble(
  readxl::read_xlsx("data/18-19.xlsx", skip=1, col_types = col_types)
  ) %>% add_column(year = "18/19")

RAW <- rbind(D1920, D1819)
rm(D1920, D1819)

# We add our own ID because the given IDs could be double (2018/19, 2019/20)
RAW$id_original <- RAW$id
RAW$id <- 1:nrow(RAW)

seasons <- tibble(
  name = c("spring", "summer", "winter"),
  rcol = c("0[1-2]", "0[3-7]", "0[8-9]"),
  short = c("SP", "SU", "WI"),
  desc = c("SPRING", "SUMMER", "WINTER")
)

# Temporary for Combinations
RAW$c_short <- NA
RAW$c_desc  <- NA

for(i in 1:nrow(seasons)){
  #print(paste0("Season: ", seasons$name[i]))
  for(j in 1:nrow(treatmentList)){
    #print(paste0("Treatment: ", treatmentList$tname[j]))
    treatmentexp <- paste(
      "(", treatmentList$tsingle[j], ")\\S*", seasons$rcol[i], 
      sep = ""
      )
    # Get Columns which are starting with List value
    x       <- grepl(treatmentexp, colnames(RAW), fixed = FALSE, perl = TRUE)
    rsums   <- rowSums(RAW[, x], na.rm = TRUE)
    colname <- paste(treatmentList$ttotal[j], seasons$name[i], sep = "_")
    # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
    RAW <- RAW %>% add_column(
      !!(colname) := rsums
    )
    # create a yes no list too
    colnameyn <- paste( treatmentList$ttotal[j], "yn_", seasons$name[i],  sep = "")
    RAW <- RAW %>% add_column(
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

RAW$c_short <- stringr::str_replace(RAW$c_short, "NA &", "")
RAW$c_desc  <- stringr::str_replace(RAW$c_desc, "NA &", "")

rm(seasons, colname, colnameyn, i, j, rsums, treatmentexp, x, vlogical, colshort, coldesc)

#### TOTAL Treatment Values ####

# Temporary for Combinations, without seasons
RAW$t_short      <- NA
RAW$t_desc       <- NA
RAW$t_estimated  <- 0
RAW$t_short_number <- NA

# Loop through our Treatment Types
for(i in 1:nrow(treatmentList)){
  # Get Columns which are starting with List value
  # double blackslash otherwise R wont escape the backslash
  # WE USE HERE all 12 months for spring, summer, winter we only use 01-10 months!
  treatmentexp10 <- paste(
    "(", treatmentList$tsingle[i], ")\\S*0[1-9]|(", treatmentList$tsingle[i], ")\\S*1[0]", 
    sep = ""
    )
  treatmentexp12 <- paste(
    "(", treatmentList$tsingle[i], ")\\S*0[1-9]|(", treatmentList$tsingle[i], ")\\S*1[0-2]", 
    sep = ""
    )
  # sum the row values (means 1 = for 1 month, 2 = 2 months etc.)
  x10 <- grepl(treatmentexp10, colnames(RAW), fixed = FALSE)
  rsum10 <- rowSums(RAW[, x10], na.rm = TRUE)
  x12 <- grepl(treatmentexp12, colnames(RAW), fixed = FALSE)
  rsum12 <- rowSums(RAW[, x12], na.rm = TRUE)
  
  RAW <- RAW %>% add_column(
    !!(treatmentList$ttotal[i]) := rsum10,
    !!(paste(treatmentList$ttotal[i], "12", sep="")) := rsum12,
  )
  
  #D.CACHE[[i[2]]] <- rowSums(RAW[, x10], na.rm = TRUE)
  #D.CACHE[[paste(i[2], "12", sep = "")]] <- rowSums(RAW[, x12], na.rm = TRUE)
  # create a yes (1) no (2) list too
  colnameyn <- paste( treatmentList$ttotal[i], "_yn",  sep = "")
  RAW <- RAW %>% add_column(
    !!(colnameyn) := ifelse(rsum10 > 0, 1, 0),
  )
  
  # Combination
  if(i == 1) next # dont add vcontrol to combination
  vlogical <- as.logical(rsum12)
  
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
  
  if(i == 2) next # dont add to number combination
  
  RAW$t_short_number[vlogical] <- paste(
    RAW$t_short_number[vlogical],
    paste(rsum12[vlogical], treatmentList$tshort[i], sep="-"), 
    sep = " & "
  )
  
}

RAW$t_short <- stringr::str_replace(RAW$t_short, "NA &", "")
RAW$t_desc  <- stringr::str_replace(RAW$t_desc, "NA &", "")
RAW$t_short_number  <- stringr::str_replace(RAW$t_short_number, "NA &", "")

RAW$t_estimated <- round(RAW$t_estimated, 2)

rm(colnameyn, i, rsum10, rsum12, treatmentexp10, treatmentexp12, x10, x12, estimated, vlogical)

# sum rows for total different methods and SEASONS
# sum rows by yn column, that way we get amount of different treatments used
x <- grep("(yn_)", colnames(RAW), fixed = FALSE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_totalyn_)", colnames(RAW), fixed = FALSE)
x <- x[!(x %in% t)]
RAW$T_season <- rowSums(RAW[, x], na.rm = TRUE)

# sum rows by yn column, that way we get amount of different treatments used
x <- grep("_yn", colnames(RAW), fixed = TRUE)
# Exclude Varroa Control from Treatment Counts
t <- grep("(vcount_total_yn)", colnames(RAW), fixed = FALSE)
x <- x[!(x %in% t)]
RAW$T_amount <- rowSums(RAW[, x], na.rm = TRUE)

#### Overrule "wrong" User Input for varroa checked and treated question
RAW$varroa_checked[RAW$varroa_checked != "Ja" & RAW$T_vcount_total12 > 0] <- "Ja"
RAW$varroa_treated[RAW$varroa_treated != "Ja" & RAW$T_amount > 0]         <- "Ja"

### Generate Cols without Drone Brood Removal
RAW$t_desc_od  <- RAW$t_desc %>% str_replace(., "Drone brood removal & ", "") %>% str_trim()
RAW$t_short_od <- RAW$t_short %>% str_replace(., "Drone & ", "") %>% str_trim()
# REMOVES cols not needed
RAW <- RAW %>% select(
  -starts_with(c("lost_", "symp_", "flow_")),
  -c("apiary_nearby", "weak", "young_queens", 
      "hives_spring_before", "queen_problems",
      "op_mash_bottom_board", "op_insulated_hives",
      "op_plastic_hives")
  ) %>% 
  select(-c(op_varroatolerant:crippled_bees)) %>% 
  select(-c(T_vcount_01:T_other_12))

rm(t, x)