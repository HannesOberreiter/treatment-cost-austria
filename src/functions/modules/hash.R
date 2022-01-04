# Salt and Hashing Personal Data
library(digest)
fHash <- function(x) {
    salt <- Sys.getenv("SALT")
    x <- paste0(x, salt)
    vapply(x, digest::digest, FUN.VALUE = "", USE.NAMES = FALSE)
}