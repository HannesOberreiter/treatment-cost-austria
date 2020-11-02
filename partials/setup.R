knitr::opts_chunk$set(
  fig.pos   = "H", 
  out.extra = "", 
  fig.align = "center", 
  out.width = "100%",
  echo      = FALSE,
  include   = FALSE
)

# Our Libraries
VLIBS <- c(
  "tidyverse", "patchwork", "here", 
  "readxl", "bookdown", "BlandAltmanLeh", 
  "boot", "ggsignif", "lme4", 
  "rgeos", "maptools", "rgdal",
  "ggstatsplot"
  )

# Use Colors in Plots
# https://stackoverflow.com/questions/42458412/plotting-data-by-color-variable-with-ggplot2-in-r#comment72062654_42458412
colorBlindBlack8  <- c("#464343", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

F_LoadLibs <- function(x){
  print(x)
  if(!require(x, character.only = TRUE)) install.packages(x); library(x, character.only = TRUE);
  return(T)
}
# https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
options(dplyr.summarise.inform=F) 
# https://stackoverflow.com/questions/13286531/how-to-suppress-warnings-when-plotting-with-ggplot
options(warn=-1)
sapply(VLIBS, F_LoadLibs)
rm(VLIBS, F_LoadLibs)

#### Base Function and Data

# Beekeeper and Colonies in Austria based on 
# https://www.biene-oesterreich.at/daten-und-zahlen+2500++1000247
stats_aut <- tibble(
  year = c(2018, 2019),
  beekeeper = c(28432, 30237),
  colonies = c(373412, 390607)
)

# Bootstrap Functions for Median and Mean
fBootMedian <- function(d, i) {
  median(d[i])
}
fBootMean <- function(d, i) {
  mean(d[i])
}

# Our custom cluster function
# x = subseted Dataframe (lat, long), you need to do beforehand the logic what data you want
# number_clusters = divides the n with the given number, it tells us how many clusters we want to find
fMapCluster <- function( x, number_clusters = 2){
  # I do not really understand how else we should do this, because it is not a real "Cluster" Search
  # Creating our clustering without deep mathematics, high k-means then remove one cluster frame and only keep up to 3 when they to overlap
  x <- x %>% na.omit()
  # kmeans for simple automatic cluster search, 1/4 seems to work good with this data
  # 1/4 for beekeeper distribution
  c <- kmeans(x, ( nrow(x)/number_clusters) )
  # Extract Data from kmeans
  CENTERS <- as.data.frame( c$centers )
  CENTERS$cluster <- seq.int( nrow(CENTERS) )  
  CENTERS$n <- c$size
  CENTERS$w <- c$withinss
  CENTERS <- subset( CENTERS, n > 1 ) # remove clusters with 1 point inside, here is the actuall coords from the source better than the cluster center
  CENTERS <- subset( CENTERS, n > 3 | (n < 3 & w == 0) ) # withinss is a measure for disp. inside cluster, by small cluster sizes we want to remove them but if they stacked togheter we keep them
  # Prep. Cache
  x$cluster <- factor( c$cluster )
  x$n <- 1
  x$w <- 0
  # Compare Cache wih our Cluster to only have a cache with points which are not inside the selected cluster
  x <- x[!x$cluster %in% CENTERS$cluster,]
  x <- rbind(CENTERS, x)
  # Arrange DF by n, because if overlapping happens the darkest point (most n) will be at top (example vienna)
  x <- x %>%
    arrange(n)
  return(x)
}