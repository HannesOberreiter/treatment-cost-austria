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
