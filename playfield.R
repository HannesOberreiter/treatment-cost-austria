dplyr::glimpse(DATA)

# Generate Bootstrap Dataset
x <- boot::boot(DATA$costs, fBootMedian, R=10000); x
y <- boot::boot(DATA$costs, fBootMean, R=10000); y
# Plot Distribution
plot(x)
main="Distribution - Median Bootstrap"
x$t0

p <- fBootPlot(y, "Mean")
p

max(x$t)

plot(y, main="Distribution - Mean Bootstrap")
# Calculate CI
xci <- boot::boot.ci(x, conf=0.95, type="bca")
yci <- boot::boot.ci(y, conf=0.95, type="bca")

xci

