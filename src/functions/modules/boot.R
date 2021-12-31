# Bootstrap Functions for Median and Mean
fBootMedian <- function(d, i) {
    median(d[i])
}
fBootMean <- function(d, i) {
    mean(d[i])
}
fBootGeomMean <- function(d, i) {
    d[d == 0] <- 0.01
    exp(mean(log(d[i])))
}
