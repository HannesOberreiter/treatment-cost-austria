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


# PCA ---------------------------------------------------------------------



fGLM <- function(d){
  GLM.FULL <- glm(
    cbind( hives_lost_e, hives_spring_e ) ~ t_short_od_lump,
    family = quasibinomial( link = "logit" ), 
    data = d, na.action = na.omit
  )
  SUMMARY.FULL <- summary( GLM.FULL )
  print( SUMMARY.FULL ) 
  # F-Test
  ANOVA.FULL <- anova( GLM.FULL, test = "F" )
  print( ANOVA.FULL )
  # Chi-Sqr Test
  ANOVA.CHISQ <- anova( GLM.FULL, test = "Chisq" )
  print( ANOVA.CHISQ )
  
  # Calculate odds via model for factors  
  DFRESIDUAL.FULL <- SUMMARY.FULL$df.residual
  VALUES.FULL <- predict(
    GLM.FULL, 
    tibble(t_short_od_lump = levels(GLM.FULL$data$t_short_od_lump)),
    type = "link", se.fit = T
  )
  
  # Approximate 95% CIs for the log odds & upper and lower limit (NOT WITH REFERENCE REGION, Dunno)
  CACHE.ODDS <- VALUES.FULL$fit
  CACHE.LOWERLIM <- VALUES.FULL$fit - qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  CACHE.UPPERLIM <- VALUES.FULL$fit + qt( 0.975, df = DFRESIDUAL.FULL ) * VALUES.FULL$se.fit
  # Add Prob. to our plot matrix
  CACHE.ODDS =inv.logit( CACHE.ODDS ) * 100 
  CACHE.LOWERLIM = inv.logit( CACHE.LOWERLIM ) * 100 
  CACHE.UPPERLIM = inv.logit( CACHE.UPPERLIM ) * 100 
  
  CACHE.BIND <- tibble(
    levels = levels(GLM.FULL$data$t_short_od_lump),
    lowerlim = CACHE.LOWERLIM,
    middle = CACHE.ODDS,
    upperlim = CACHE.UPPERLIM
  )
  return(CACHE.BIND)
}

pc1920 <- DATA %>% 
  filter(year == "19/20") %>% 
  mutate(
    t_short_od_lump = fct_drop(t_short_od_lump)
  )
pc1920 <- pc1920 %>% 
  group_by(year, t_short_od_lump) %>% 
  summarise(
    n = n(),
    mean = mean(costs),
    median = median(costs),
    mad = mad(costs),
    sd = sd(costs)
  ) %>% 
  bind_cols(fGLM(pc1920))

pc1819 <- DATA %>% 
  filter(year == "18/19") %>% 
  mutate(
    t_short_od_lump = fct_drop(t_short_od_lump)
  )
pc1819 <- pc1819 %>% 
  group_by(year, t_short_od_lump) %>% 
  summarise(
    n = n(),
    mean = mean(costs),
    median = median(costs),
    mad = mad(costs),
    sd = sd(costs)
  ) %>% 
  bind_cols(fGLM(pc1819))

pc <- bind_rows(pc1920, pc1819)

# breakline for costs both years together? Maybe makes more sense
dc_sum <- pc %>% summarise(
  total_mean = mean(DATA$costs),
  total_median = median(DATA$costs),
  total_middle = mean(middle)
)

pc <- pc %>% left_join(dc_sum, by = c("year" = "year"))

pc <- pc %>% mutate(
  type = case_when(
    median <= total_median & middle < total_middle ~ "Cheap and Good",
    median > total_median & middle > total_middle ~ "Expensive and Bad",
    median <= total_median & middle > total_middle ~ "Cheap and Bad",
    median > total_median & middle < total_middle ~ "Expensive and Good"
  )
)

library(ggrepel)

pcPlot <- pc %>% 
  #ggplot(aes(x = mean, y = middle, color = type)) +
  ggplot(aes(x = median, y = middle, color = type)) +
  geom_label_repel(aes(label=t_short_od_lump), max.overlaps = 30) +
  geom_hline(data = dc_sum, aes(yintercept = total_middle), size = 1, color = "black") +
  geom_vline(data = dc_sum, aes(xintercept = total_mean), size = 1, color = "red") +
  geom_vline(data = dc_sum, aes(xintercept = total_median), size = 1, color = "black") +
  geom_point(show.legend = F) +
  geom_errorbar(aes(ymin = lowerlim, ymax = upperlim), show.legend = F, alpha = 0.2) +
  #geom_errorbarh(aes(xmin = mean-sd, xmax = mean+sd), show.legend = F, alpha = 0.2) +
  geom_errorbarh(aes(xmin = median-mad, xmax = median+mad), show.legend = F, alpha = 0.2) +
  scale_y_continuous(
    breaks = seq(0, 50, 1)
  ) +
  scale_x_continuous(
    breaks = seq(0, 15, 1)
  ) +
  scale_color_manual(
    values = colorBlindBlack8, name="Survey"
  ) +
  coord_cartesian(xlim = c(0,15), ylim = c(0,40), expand = FALSE) +
  facet_wrap(~ year)

fSaveImages("stats-loss-cost-plot", pcPlot)










