dplyr::glimpse(DATA)
str(DATA)



DATA$t_short_od <- as.factor(DATA$t_short_od)
d <- DATA %>% group_by(t_short_od) %>% filter(n() > 15)

x <- aov(d$costs ~ d$t_short_od)
summary(x)
summary.lm(x)

d$id

kruskal.test(costs ~ t_short_od, data=d)

DATA$hive_lost_p = DATA$hives_lost / DATA$hives_winter
d$hive_lost_p = d$hives_lost / d$hives_winter

one.way <- lmer(hive_lost_p ~ 1 + (1 | t_short_od) + (1 | costs) + (1 | t_short_od:costs), data=DATA)
one.way <- lmer(costs ~ t_short_od + (t_short_od | hive_lost_p), data=DATA)
one.way <- lmer(costs ~ t_short_od + (1 | hive_lost_p), data=d)

one.lm <- lm(costs ~ t_short_od, data = d)
summary(one.lm)
plot(one.lm)

summary(one.way)
one.wayML <- update(one.way, REML=FALSE)
summary(one.wayML)

ranef(one.way)
plot(ranef(one.way, condVar=TRUE))
vcov(one.way)
plot(one.way)
confint(one.way)

one.lm <- lm(costs ~ 1, data = DATA)
summary(one.lm)
confint(one.lm)




# tukeyhsd assumes same n for each factor
TukeyHSD(aov(DATA$costs ~ DATA$size_group))



### Relative Kosten, großökonomische Kosten

DATA$cost_colonies = DATA$costs * DATA$hives_winter

summary(DATA$cost_colonies)
sum(DATA$cost_colonies)


s <- DATA %>% group_by(size_group) %>%  summarise(
  n = n(),
  n_hives = sum(hives_winter),
  summe = sum(cost_colonies)
)
 
s$percent <- s$summe / sum(s$summe) * 100

# Die Summe auch mit Mean + CI ausrechnen und schauen ob es ziemlich es gleiche ist. 

max(DATA$hives_winter)

## GLM

g1 <- glm(
  costs ~ hives_winter * t_short_od, data = DATA, family = gaussian()
)
summary(g1)

g2 <- glm(
  costs ~ hives_winter + t_short_od, data = DATA, family = gaussian()
)

g3 <- glm(
  costs ~ hives_winter, data = DATA, family = gaussian()
)

g4 <- glm(
  costs ~ hives_winter, data = DATA, family = gaussian()
)
summary(g4)
confint(g4)
plot(g4)
range(DATA$hives_winter)
xhives <- seq(0, 1000, 1)
x

ycost <- predict(g4, list(hives_winter = xhives), type = "response")
lot(g4)

plot(DATA$hives_winter[DATA$hives_winter < 50], DATA$costs[DATA$hives_winter < 50], pch = 16)
lines(xhives, ycost)

a <- anova(g2, g3)
a


plot(g1)

plot(predict.glm(g2))


### GGStatsPlot

x <- DATA[DATA$costs < 100,]

ggstatsplot::ggbetweenstats(
  data = x,
  x = size_group,
  y = costs,
  mean.ci = TRUE,
  
) + xlab("Operation Size") + ylab("Cost/Colony [Euro]")


glm_size_gaussian <- glm(
  costs ~ 0 + size_group * t_short_od, data = DATA, family = gaussian()
)

ggstatsplot::ggcoefstats(
  x = glm_size_gaussian
  )

one.way <- lmer(
  costs ~ 0 + size_group + (1 | t_short_od), data=DATA)
ranef(one.way)

x <- lm(costs ~ 0 + t_short_od + size_group, DATA)
y <- lm(costs ~ 0 + t_short_od, DATA)
a <- lm(costs ~ 0 + size_group, DATA)

anova(x,y,a)
summary(x)


summary(one.way)
confint(one.way)
anova(one.way)


ggstatsplot::ggcoefstats(
  x = one.way
)

x <- grep("_yn", colnames(RAW), fixed = TRUE)

glimpse(DATA[, c(1, 3, x)])

y <- DATA[, c(x, 399)]
ca <- DATA[, x]
c <- aggregate(.~size_group, y, sum)

y_g <- y %>% group_by(size_group) %>% summarise(n = n())

c <- c %>% add_column(y_g$n)
 
c[,2:14] <- c[,2:14] / c$`y_g$n` * 100

res.pca <- prcomp(c[,2:14], scale = TRUE)


y = DATA[, c(1, 3, x, 399)]
res2.pca <- prcomp(y[,-c(1,2, 3, 16)], scale = TRUE)

plot(res2.pca)
res.pca

res2.pca

factoextra::fviz_pca_biplot(
  res2.pca,
  label="var",
  habillage=y$state,
  addEllipses=TRUE, ellipse.level=0.95,
  col.var = "#2E9FDF", # Variables color
  col.ind = "#696969"  # Individuals color
)


head(DATA[])

library("mvnormtest")

as.matrix(DATA$costs, DATA$hives_lost)

mshapiro.test(c(DATA$costs, DATA$hives_lost))

# MANOVA


d <- DATA %>% group_by(t_short_od) %>% filter(n() > 15)

c <- d$costs
l <- d$hives_lost / d$hives_winter

res.man <- manova(cbind(c, l) ~ d$size_group + d$t_short_od, data = d)
res.man
summary(res.man)
summary.aov(res.man)

result = NULL
for(i in c(1:30)){
  dummy <- df %>% group_by(country) %>% sample_n(100)
  dummy$country <- paste0(dummy$country, i)
  result = bind_rows(result, dummy)
}

result %>% group_by(country) %>% summarise(
  sum = logic für deine collumns
)












