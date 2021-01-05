
# Bootstrap Functions for Median and Mean
fBootMedian <- function(d, i) {
  median(d[i])
}
fBootMean <- function(d, i) {
  mean(d[i])
}

# simple function to save images 
fSaveImages <- function(filename, currentplot, w = 7.5, h = 4){
  ggsave(paste0("images/", filename, ".pdf"), currentplot, width = w, height = h)
  ggsave(paste0("images/", filename, ".png"), currentplot, width = w, height = h, dpi = 320)
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


# Kruskall ####
# COIN Permutation Test
# kruskal_wallis without coin gives the same test results
# effekt size is based on coin kruskal wallis, if we use bootstrap we can get 
# CI, effektsize is multiplied by 100 the % explained by the variable of the variance

fCoinKruskal <- function(depentend, indepentend){
    x <- tibble(dep = depentend, ind = indepentend)
    coin::kruskal_test(
      dep ~ 0 + ind, data = x, 
      distribution = approximate(
        nresample = 10000, parallel = "multicore"
      )
    )
}

# Effect Size ####
# The interpretation values commonly in published literature are: 
# 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).
fEffektSize <- function(depentend, indepentend){
  x <- tibble(dep = depentend, ind = indepentend)
  kruskal_effsize(
    dep ~ 0 + ind, data = x, 
    ci = T, ci.type = "bca", nboot = 5000, parallel = "multicore", ncpus = parallel::detectCores()
  )
}


fCoinLabel <- function(input, eff){
  df   <- resKruskal %>% map_int(~ .x@statistic@df)
  stat <- resKruskal %>% map_dbl(~ statistic(.x))
  p    <- resKruskal %>% map_chr(~ p_format(pvalue(.x), accuracy = 1e-03 ))
  print(df)
  print(stat)
  print(p)
  TeX(
    sprintf(
      '$\\chi^2$(%i) = $%.2f$, $\\textit{p}$ = $%s$, $\\eta^2(H)$ = $%.2f$', 
      df,
      stat,
      p,
      eff$effsize
    )
  )
}

fPairwiseMM <- function(d){
  x = tibble(
    xmin = character(), xmax = character(), 
    mean_diff = numeric(), median_diff = numeric()
  )
  for(i in 1:(nrow(d)-1)){
    xmin    <- d[i, 1]
    meani   <- d[i, 3]
    mediani <- d[i, 4]
    for(j in (i+1):nrow(d)){
      xmax    <- d[j, 1]
      meanj   <- d[j, 3]
      medianj <- d[j, 4]
      
      dummy = cbind(
        xmin = xmin, xmax = xmax, 
        mean_diff = round(abs(meani - meanj), 1),
        median_diff = round(abs(mediani - medianj))
      )
      names(dummy) <- c("xmin", "xmax", "mean_diff", "median_diff")
      x = x %>% add_row(dummy)
    }
  }
  x <- x %>% add_column(
    year = d$year[1],
    tex = TeX(
        sprintf(
          '$\\Delta | \\tilde{x} |$ = $%.1f$',
          x$median_diff
        )
      )
  )
  return(x)
}
