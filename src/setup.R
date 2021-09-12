# Description -------------------------------------------------------------
# Loading of Libraries, settings and some standard constants (eg. colors)

# knitr Setup -------------------------------------------------------------
knitr::opts_chunk$set(
  tidy = FALSE,
  fig.pos = "H",
  out.extra = "",
  fig.align = "center",
  out.width = "100%",
  fig.width = 6,
  fig.height = 4,
  graphics.auto_pdf = TRUE,
  echo = FALSE,
  include = FALSE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE,
  verbose = TRUE,
  comment = NA
)

set.seed(1337) # fix random seed for models and subsampling
# Libraries ---------------------------------------------------------------
# igraph installation problem: https://github.com/igraph/rigraph/issues/135 (dependency of solitude)
libs <- c(
  "tidyverse", "here", "readxl", "janitor", # Main Packages
  "glue", "patchwork", "rlang", "lintr",
  "scales", "ggforce", "ggdist", "gghalves",
  "isotree",
  "knitr", "bookdown", "kableExtra", # Main .Rmd Packages
  "latex2exp", "ggsignif", "ggupset", "ggrepel", # Helper Packages
  "sf", # Map
  "BlandAltmanLeh", "boot", "coin" # Statistics
)

# sf library needs to be loaded directly and not from cache
# we do not use RENV for version control anymore as it did cause some problems
# library("sf", lib.loc = "/usr/local/lib/R/4.0/site-library/")
# library("coin", lib.loc = "/usr/local/lib/R/4.0/site-library/")

# Load Libraries with function, install binary if not installed (mac binaries are defined!)
# x = Libraries name as String
fLoadLibs <- function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, type = "mac.binary")
  library(x, character.only = TRUE)
  return(T)
}

sapply(libs, fLoadLibs)
rm(libs, fLoadLibs)

# Misc Settings -------------------------------------------------------
# https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
options(dplyr.summarise.inform = F)
# https://stackoverflow.com/questions/13286531/how-to-suppress-warnings-when-plotting-with-ggplot
options(warn = -1)
# Increase base font size for our plots
theme_set(theme_classic(base_size = 12))

# Constants ---------------------------------------------------------------
# Color Definition
# https://stackoverflow.com/questions/42458412/plotting-data-by-color-variable-with-ggplot2-in-r#comment72062654_42458412
colorBlindBlack8 <- c(
  "#464343", "#E69F00", "#56B4E9", "#009E73",
  "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
)



# Next Files --------------------------------------------------------------
source("src/partials/list-motivation.R", local = knitr::knit_global())
source("src/partials/list-austria.R", local = knitr::knit_global())
source("src/partials/list-treatments.R", local = knitr::knit_global())
source("src/partials/standard-cost.R", local = knitr::knit_global())
source("src/partials/files.R", local = knitr::knit_global())
source("src/functions/functions.R", local = knitr::knit_global())