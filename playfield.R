# TODO:
# to test if I can combine the two years
# I simply test if they are different
# in model test I can also use year as factor cost ~ factor * year
# to check if it improves the result
# also we need to convert costs to log because of non norm data
# log(cost + 1) +1 is needed because of zero answers

# machine learning with months + treatment to check for costs ?

dplyr::glimpse(DATA)
#  broom::tidy(costs_size_pairwise)

# TukeyHSD assumes same n for each factor, therefore we use Dunn
TukeyHSD(aov(DATA$costs ~ DATA$size_group))

# lmer mixed random effect model 
# not really useful in my case though? Fist I would need
# to transform costs as it is normal distributed 
# eg. log(costs + 1/6)
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

# linear regression, same as above
one.lm <- lm(costs ~ 1, data = DATA)
summary(one.lm)
confint(one.lm)

### Relative Kosten, großökonomische Kosten
### what to take
# mean? not really possible
# median + what? bootstrap CI?
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

## GLM, same as mixed effect
## problem with costs not normal
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


DATA$costs_log <- log(DATA$costs + 1)
glm_size_gaussian <- glm(
  costs_log ~ 1 + year, data = DATA, family = gaussian()
)
summary(lm( costs_log ~ year, data = DATA))
confint(rlm)
t.test(
  DATA$costs_log[DATA$year == "18/19"],
  DATA$costs_log[DATA$year == "19/20"]
  )

DATA$year <- as.factor(DATA$year)
DATA$state <- as.factor(DATA$state)

model1 <- glm(costs_log ~ state * year, data=DATA)
model2 <- glm(costs_log ~ state, data=DATA)
anova(model1, model2, test="LRT")


summary(m)
ggplot(fortify(m), aes(state, costs_log, color = year)) +
  stat_summary(fun.data=mean_se, geom="pointrange") +
  stat_summary(aes(y=.fitted), fun.y=mean, geom="line") +
  theme_classic()

hist(DATA$costs_log)





















