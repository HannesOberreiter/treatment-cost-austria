
# Our custom cluster function
# x = subseted Dataframe (lat, long), you need to do beforehand the logic what data you want
# number_clusters = divides the n with the given number, it tells us how many clusters we want to find
fMapCluster <- function(x, number_clusters = 2) {
    # I do not really understand how else we should do this, because it is not a real "Cluster" Search
    # Creating our clustering without deep mathematics, high k-means then remove one cluster frame and only keep up to 3 when they to overlap
    x <- x %>%
        select(c("latitude", "longitude")) %>%
        na.omit()
    # kmeans for simple automatic cluster search, 1/4 seems to work good with this data
    # 1/4 for beekeeper distribution
    c <- kmeans(x, (nrow(x) / number_clusters))
    # Extract Data from kmeans
    CENTERS <- as.data.frame(c$centers)
    CENTERS$cluster <- seq.int(nrow(CENTERS))
    CENTERS$n <- c$size
    CENTERS$w <- c$withinss
    CENTERS <- subset(CENTERS, n > 1) # remove clusters with 1 point inside, here is the actuall coords from the source better than the cluster center
    CENTERS <- subset(CENTERS, n > 3 | (n < 3 & w == 0)) # withinss is a measure for disp. inside cluster, by small cluster sizes we want to remove them but if they stacked togheter we keep them
    # Prep. Cache
    x$cluster <- factor(c$cluster)
    x$n <- 1
    x$w <- 0
    # Compare Cache with our Cluster to only have a cache with points which are not inside the selected cluster
    x <- x[!x$cluster %in% CENTERS$cluster, ]
    x <- rbind(CENTERS, x)
    # Arrange DF by n, because if overlapping happens the darkest point (most n) will be at top (example vienna)
    x <- x %>%
        arrange(n)
    return(x)
}