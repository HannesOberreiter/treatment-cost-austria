# TODO:
# To calculate the total population
# maybe "bootstrap" from our disitrubtion to
# all colonies in austria (pick one random for each colonie and replace)
# do this ~ 10 times and then we also have a range


# path diagram of model?


# for hypothesis testing if a treatment method explain different cost


# to test if I can combine the two years
# I simply test if they are different
# in model test I can also use year as factor cost ~ factor * year
# to check if it improves the result
# also we need to convert costs to log because of non norm data
# log(cost + 1) +1 is needed because of zero answers

# machine learning with months + treatment to check for costs ?


It’s difficult to come up with rules for when to use what type of model. 

Things like the study design, the nature of the outcome variable, the type of research question and what the model will be used for play a role in making a choice of model type.

You may be able to come up with some loose guidelines to keep in mind. 

For example, tree models and random forest (and other “black box” types of models) are best used when you are interested in using the model for predictive purposes and you may also have significantly more predictors than number of subjects/items in your study: p >> n.  Here, p = number of predictors and n = number of study subjects/items.

The models below would be used when you have p < n and can accommodate both prediction and explanation. (Explanation refers to describing the relationships between particular predictors and the outcome variable.)

Linear regression models can be used when you measure a continuous variable once for each study subject/item. 

Poisson regression models can be used when you measure a count variable once for each study subject/item.  With these models, you have to check for the presence of overdispersion or underdispersion. If overdispersion is present, you can swap the model for a quasipoisson model or a negative binomial model. 
Another problem you can encounter with count outcome variables is zero-inflation.  For that, you would need to use a zero-inflated or hurdle version of the Poisson or Negative Binomial regression models, depending on what mechanism you are willing to assume for generating the zeroes.
For binary outcomes, you would use binary logistic regression.

For nominal outcomes (having more than 2 unordered categories), you would use multinomial regression.

For ordinal outcomes (having more than 2 ordered categories), you would use multinomial regression.

For ‘discrete’ proportion outcomes (where you know both the numerator and denominator of the proportions and they consist of counts), you would use binomial logistic regression.

For ‘discrete’ proportion outcomes (where you know both the numerator and denominator of the proportions and they consist of counts), you would use binomial logistic regression.
For ‘continuous’ proportion outcomes, you would use beta regression or zero and/or one inflated versions.

All of these p < n models can also be used in a slightly different context: when you have a single study subject/item (e.g., country) and you measure the values of that subject/item repeatedly over time.

For situations where the outcome value is measured multiple times for at least some of the study subjects/items and you have several such such subjects/items, you would use mixed effects versions of the p < n models described above.


DATA$logcost <- log10(DATA$costs+(1/6))

# https://learningstatisticswithr.com/book/bayes.html#the-p-value-is-a-lie.

library("BayesFactor")
D <- DATA[!is.na(DATA$op_cert_org_beek) & DATA$op_cert_org_beek != "Unsicher",]
D$op_cert_org_beek <- as.factor(D$op_cert_org_beek)

r <- BayesFactor::ttestBF(
  formula = logcost ~ op_cert_org_beek, data = D
)
r
BayesFactor::extractBF(r)

t <- t.test(
  formula = logcost ~ op_cert_org_beek, data = D
)
t

mean(D$costs)

chains = posterior(r, iterations = 10000)
plot(chains[,2])


models <- generalTestBF( 
  formula = logcost ~ hives_winter + year + t_short_od + op_cert_org_beek + id,
  whichRandom = "ID",
  neverExclude = "ID",
  data = DATA[!is.na(DATA$op_cert_org_beek),],
  whichModels = "top"
  )

models

amodel <- aov(
  formula = logcost ~ year + tri_size + t_short_od,
  data = DATA,
)
ares <- Anova(amodel)
ares
 



##### COIN Permutation Test
# I may use the coin kruskal_test as it is little bit better with resampling
# but kruskal_wallis without coin gives the same test results
# effekt size is based on coin kruskal wallis, if we use bootstrap we cann get 
# CI, effektsize is multiplied by 100 the % explained by the variable of the varianz

library(coin)
resEffect1819 <- DATA %>% filter(year == "18/19") %>% 
  kruskal_effsize(
    costs ~ 0 + tri_size,
    ci = T, ci.type = "bca", nboot = 5000
    ) %>% 
  add_column(year = "18/19")

res <- DATA %>% filter(year == "18/19") %>% 
  coin::kruskal_test(
    costs ~ 0 + tri_size, data = ., 
    conf.int = TRUE,
    distribution = approximate(
      nresample = 10000, parallel = "multicore", ncpus = 4
      )
    )

res2 <- DATA %>% filter(year == "18/19") %>% 
  rstatix::kruskal_test(costs ~ 0 + tri_size)

confint(res)
statistic(res, type = "linear")
expectation(res)
pvalue(res)


DATA %>% group_by(tri_size) %>% get_summary_stats(costs, type = "full")
?MAD

l = TeX(
  sprintf(
    'Approximative Kruskal-Wallis, $\\chi^2$(%i) = $%.2f$, $\\textit{p}$ = $%s$', 
    res@statistic@df,
    signif(statistic(res), 4),
    p_format(pvalue(res))
    )
  )

ggplot() + ggtitle(l)


### correlation?

DATAsub <- DATA %>% 
  select(costs, op_cert_org_beek, op_migratory_beekeeper, hives_winter) %>% 
  mutate(
    #costs = log(costs),
    hives_winter = ifelse(hives_winter < 50, 0, 1)
  ) %>% 
  mutate(
    across(
      starts_with("op_"), 
      ~ dplyr::recode(.x, Ja = 1, Nein = 0, .default = 2)
    )
  ) %>% 
  filter(across(starts_with("op_"), ~ .x != 2))

cormat <- DATAsub %>% cor_test(everything())


ggplot(DATAsub, aes(op_migratory_beekeeper, op_cert_org_beek)) + geom_jitter()

cormat$co
ggplot(cormat, aes(x=var1, y=var2, fill=cor)) + 
  geom_tile()

a <- glm(
  costs ~ 1 + op_cert_org_beek + op_migratory_beekeeper + hives_winter, 
  data = DATAsub
  )
summary(a)
coef(a)
coef(a$modelStruct$corStruct, uncons = FALSE, allCoef = TRUE)
coef(a$modelStruct$varStruct, uncons = FALSE, allCoef = TRUE)

ggcorr(a)
plot(a)

mutate(
  across(
    starts_with("op_"), 
    ~ dplyr::recode(.x, Ja = 0, Nein = 1, .default = 2)
  )
) %>% filter(across(starts_with("op_"), ~ .x == 3))

DATAsub$op_cert_org_beek
DATAsub$op_migratory_beekeeper
DATAsub$
DATAsub$op_cert_org_beek







### Regression in Apiary Size

DATA$hives_winter_z <-  (DATA$hives_winter-min(DATA$hives_winter))/(max(DATA$hives_winter)-min(DATA$hives_winter))

m.lm <- lm(logcost ~ hives_winter, data = DATA)
m.lm.z <- lm(logcost ~ hives_winter_z, data = DATA)

DATA %>% map(summary)

summary(m.lm)
summary(m.lm.z)

beta <- coef(m.lm)
beta.z <- coef(m.lm.z)

l <- paste("Adj R2 = ",signif(summary(m.lm)$adj.r.squared, 5),
     "Intercept =",signif(m.lm$coef[[1]],5 ),
     " Slope =",signif(m.lm$coef[[2]], 5),
     " P =",signif(summary(m.lm)$coef[2,4], 5))

l.z <- paste("Adj R2 = ",signif(summary(m.lm.z)$adj.r.squared, 5),
           "Intercept =",signif(m.lm.z$coef[[1]],5 ),
           " Slope =",signif(m.lm.z$coef[[2]], 5),
           " P =",signif(summary(m.lm.z)$coef[2,4], 5))

ggplot(data = DATA, aes(x = hives_winter, y = logcost)) +
  geom_point(color = "blue") +
  geom_abline(intercept = beta[1], slope = beta[2], size = 1) +
  xlab("Colonies Winter [#]") + ylab("log_10(Expenses / Colony [Euro])") +
  labs(caption = l) +
  theme_classic()

ggplot(data = DATA, aes(x = hives_winter_z, y = logcost)) +
  geom_point(color = "blue") +
  geom_abline(intercept = beta.z[1], slope = beta.z[2], size = 1) +
  xlab("Colonies Winter [z-transformed]") + ylab("log_10(Expenses / Colony [Euro])") +
  labs(caption = l.z) +
  theme_classic()

### Stepwise
DATA$op_mi
fm1 <- glm(
  logcost ~ 
     year + op_migratory_beekeeper + op_foreign_wax + tri_size + t_short_od,
  data = DATA
  )
summary(fm1)
fm2 <- step(fm1)
formula(fm2)
summary(fm2)

ORG_SUB <- DATA %>% 
  filter(op_cert_org_beek != "Unsicher" & !is.na(op_cert_org_beek)) %>% 
  mutate(op_cert_org_beek = as.factor(op_cert_org_beek))

org <- lmer(logcost  ~ 0 + op_cert_org_beek, data = ORG_SUB)
summary(org)
r <- tidy(org, conf.int = TRUE) %>% 
  mutate(
    conf.low.i = 10^conf.low,
    conf.high.i = 10^conf.high,
    ); r
ggplot(ORG_SUB, aes(op_cert_org_beek, costs)) + geom_boxplot()






plot(org)


fm1 <- glm(choice ~ gender + amount + freq + last + first + child +
    + youth + cook + diy + art, data = BBBClub, family = binomial)


fullmod = glm(
  logcost ~ 
    year+op_cert_org_beek+op_foreign_wax+t_short_od,
  family=gaussian,
  data = DATA
  )
summary(fullmod)
nothing = glm(
  logcost ~ 
    1,
  family=gaussian,
  data = DATA
)
summary(nothing)

backwards = step(fullmod) # Backwards selection is the default

summary(backwards)

bothways = step(nothing, list(lower=formula(nothing),upper=formula(fullmod)),
         direction="both")

formula(bothways)

names(DATA)


dplyr::glimpse(DATA)
#  broom::tidy(costs_size_pairwise)

# TukeyHSD assumes same n for each factor, therefore we use Dunn
TukeyHSD(aov(DATA$costs ~ DATA$size_group))

# lmer mixed random effect model 
# not really useful in my case though? Fist I would need
# to transform costs as it is normal distributed 
# eg. log(costs + 1/6)
# THIS random effect would see the real difference for org beekeeper and non organic if we remove the effekt of 
# different treatment methods

names(DATA)

D <- DATA %>% filter(op_cert_org_beek != "Unsicher")
D$id
df <- lmer(log(costs+0.1) ~ 0 + op_cert_org_beek + (0 + op_cert_org_beek|t_short_od) + (0 + op_cert_org_beek|hives_winter) + (0 + op_cert_org_beek|district), data = D)
summary(df)
plot(df)
df <- lmer(log(costs+0.1) ~ 0 + op_cert_org_beek + (1+op_cert_org_beek|year) + (1+op_cert_org_beek|hives_winter), data = DATA)
summary(df)
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


## Bio Imker


# Kruskall ####
resKruskal1819 <- DATA %>% filter(year == "18/19") %>% 
  kruskal_test(costs ~ 0 + op_cert_org_beek) %>% add_column(year = "18/19")
resKruskal1920 <- DATA %>% filter(year == "19/20") %>% 
  kruskal_test(costs ~ 0 + op_cert_org_beek) %>% add_column(year = "19/20")
resKruskall <- rbind(resKruskal1819, resKruskal1920)
rm(resKruskal1819, resKruskal1920)
resKruskall

resEffect1819 <- DATA %>% filter(year == "18/19") %>% 
  kruskal_effsize(costs ~ 0 + op_cert_org_beek) %>% add_column(year = "18/19")
resEffect1920 <- DATA %>% filter(year == "19/20") %>% 
  kruskal_effsize(costs ~ 0 + op_cert_org_beek) %>% add_column(year = "19/20")
resEffect <- rbind(resEffect1819, resEffect1920)
rm(resEffect1819, resEffect1920)
resEffect

pwc1819 <- DATA %>% filter(year == "18/19") %>% 
  rstatix::dunn_test(costs ~ 0 + op_cert_org_beek, p.adjust.method = "holm") %>% 
  add_column(year = "18/19")
pwc1920 <- DATA %>% filter(year == "19/20") %>% 
  rstatix::dunn_test(costs ~ 0 + op_cert_org_beek, p.adjust.method = "holm") %>% 
  add_column(year = "19/20")

resPWC <- rbind(pwc1819, pwc1920) %>% rstatix::add_xy_position(x = "tri_size")


# Plot Statistical Test ####
facetLabels <- tibble(
  year = c("18/19", "19/20"),
  label = c(
    get_test_label(
      resKruskall[1,], detailed = TRUE
    ),
    get_test_label(
      resKruskall[2,], detailed = TRUE
    ))
)
plotOperationStat <- DATA %>% filter(costs < 200 & !is.na(op_cert_org_beek)) %>% 
  ggplot(.,
         aes(
           y = costs,
           x = op_cert_org_beek,
           color = year
         )) +
  geom_boxplot(show.legend = F) +
  theme_classic() + ylab("Expenses/Colony [Euro]") + xlab("Certified Organic Beekeeper") +
  theme(
    axis.text.x = element_text(size = 14)
  ) +
  labs(
    caption = str_replace(
      get_pwc_label(
        resPWC, type="text"
      ), 
      "pwc", "Pairwise Comparisons"
    )
  ) +
  ggsignif::geom_signif(
    data=resPWC,
    aes(
      xmin=xmin, xmax=xmax, 
      annotations=p.adj.signif, 
      y_position = c(75,80,87, 75,80,87), 
      group = p.adj
    ),
    textsize = 3, color = "black", manual=TRUE, parse=FALSE
  ) +
  scale_y_continuous(
    limits = c(0,100),
    breaks = c(seq(0,100,5))
  ) + 
  scale_color_manual(
    values = colorBlindBlack8, name="Survey"
  ) +
  geom_text(
    data = facetLabels, 
    aes(label=label, x = "Nein"), 
    parse=T, y = 100, inherit.aes = F,
    size = 3
  ) +
  facet_wrap(
    ~year,
    labeller = labeller(
      year = function(x){return(paste("Survey", x))}))
plotOperationStat


