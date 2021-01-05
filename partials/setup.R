knitr::opts_chunk$set(
  tidy      = FALSE,
  fig.pos   = "H", 
  out.extra = "", 
  fig.align = "center", 
  out.width = "100%",
  fig.width = 6, 
  fig.height = 4,
  graphics.auto_pdf = TRUE,
  echo      = FALSE,
  include   = FALSE,
  warning   = FALSE,
  message   = FALSE, 
  cache     = FALSE,
  verbose   = TRUE,
  comment   = NA
)

# Our Libraries
VLIBS <- c(
  "tidyverse", "patchwork", "here", 
  "readxl", "bookdown", "BlandAltmanLeh", 
  "boot", "ggsignif", "lme4", "ggupset",
  "kableExtra", "rstatix", "latex2exp", "coin", "sf"
  )

# sf library needs to be loaded directly and not from cache
#library("sf", lib.loc = "/usr/local/lib/R/4.0/site-library/")
#library("coin", lib.loc = "/usr/local/lib/R/4.0/site-library/")

# Use Colors in Plots
# https://stackoverflow.com/questions/42458412/plotting-data-by-color-variable-with-ggplot2-in-r#comment72062654_42458412
colorBlindBlack8  <- c("#464343", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

F_LoadLibs <- function(x){
  print(x)
  if(!require(x, character.only = TRUE)) install.packages(x, type = "mac.binary"); library(x, character.only = TRUE);
  return(T)
}
# https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
options(dplyr.summarise.inform=F) 
# https://stackoverflow.com/questions/13286531/how-to-suppress-warnings-when-plotting-with-ggplot
options(warn=-1)
sapply(VLIBS, F_LoadLibs)
rm(VLIBS, F_LoadLibs)

theme_set(theme_classic(base_size=12))


#### Base Function and Data

# Beekeeper and Colonies in Austria based on 
# https://www.biene-oesterreich.at/daten-und-zahlen+2500++1000247
stats_aut <- tibble(
  year = c(2018, 2019),
  year2 = c("18/19", "19/20"),
  beekeeper = c(28432, 30237),
  colonies = c(373412, 390607)
)
