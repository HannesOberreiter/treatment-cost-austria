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