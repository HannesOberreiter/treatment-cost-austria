dplyr::glimpse(DATA$t_short_od)

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

