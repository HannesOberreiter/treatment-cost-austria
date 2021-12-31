fGeoMean <- function(x) {
    zeros <- x == 0
    if (sum(zeros) > 0) {
        print(paste("Dropped zero values: ", sum(x == 0)))
    }
    exp(mean(log(x[!zeros])))
}

fGeoSD <- function(x) {
    zeros <- x == 0
    if (sum(zeros) > 0) {
        print(paste("Dropped zero values: ", sum(x == 0)))
    }
    exp(sd(log(x[!zeros])))
}
# https://stackoverflow.com/questions/2676554/in-r-how-to-find-the-standard-error-of-the-mean
fSE <- function(x, ...) sqrt(var(x, ...) / length(x))
