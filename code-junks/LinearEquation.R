#library(matlib)
# https://cran.r-project.org/web/packages/matlib/vignettes/linear-equations.html

d <- DATA %>% group_by(t_short) %>% summarise(
  mc = mean(costs),
  nu = n()
)

apply(treatmentList[-c(1:2),], 1, function(x){
  v <- DATA %>% group_by(t_short) %>% summarise(
    !!(paste0(x['tshort'])) :=
      ifelse(
        sum(
          !!as.name(
            (paste0(
              x['ttotal'],12
            )
            )
          )
        ) == 0, 0,1)
  )
  d <<- add_column(d, v[,2])
})

# remove single combinations
#d <- d %>% filter(str_detect(d$t_short, "&"))
# remove low answers
d <- d %>% filter(nu >= 30)
# remove last columns (Chemical Producs and Other) as we don't have enough answers
d <- d[,-c(4,8, 12:14)]
# generate matrix
dma <- as.matrix(d[,-(1:3)])

# create variables
A <- dma
b <- d$mc
x <- d$t_short

#b <- c(9.19, 8.54, 11.10); A
#x <- c("AS-KZ", "Ox-sub", "Ox-pure")
x <- MASS::ginv(A) %*% b
x
# show numerically
y <- qr.solve(A, b)
y

rm(A, d, dma, x, b)

