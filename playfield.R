dplyr::glimpse(DATA)
str(DATA)


K.SEQ <- c( seq( 0, 100, 20 ), Inf )
K.SEQ2 <- c( seq( 20, 120, 20 ), Inf )

V.LABEL <- paste(K.SEQ, K.SEQ2, sep = "-")
V.LABEL <- V.LABEL[1:6]
V.LABEL[6] <- "> 100"
V.LABEL[1] <- "1-20"

DATA$size_group <- cut( DATA$hives_winter, K.SEQ, labels=V.LABEL, include.lowest = TRUE, right = TRUE )




#DATA2 = DATA %>% mutate(tri_size = factor(tri_size, labels = c("1-10", "21-50", ">50")))

ggplot(DATA, aes(sample=costs)) + 
  geom_qq() + geom_qq_line(color="red") + 
  theme_classic() + 
  facet_wrap(
    ~tri_size, ncol = 3,
    labeller = labeller(
      tri_size = function(x){return(paste(x, "Colonies"))}
    )
  )

# https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/
# https://towardsdatascience.com/a-gentle-guide-to-statistics-in-r-ccb91cc1177e
# library("rstatix")
DATA$tri_size <- "1-20"
DATA$tri_size[DATA$hives_winter > 20] <- "21-50"
#DATA$tri_size[DATA$hives_winter > 31] <- "31-50"
DATA$tri_size[DATA$hives_winter > 50] <- ">50"
DATA = DATA %>% mutate(tri_size = factor(tri_size, levels = c("1-20", "21-50", ">50")))

n_p = DATA %>% group_by(tri_size, year) %>% summarise(
  hives = sum(hives_winter),
  participants = n(),
  mean_costs = mean(costs),
  median_costs = median(costs)
) %>% ungroup()
n_p
res.kruskal1819 <- DATA %>% filter(year == "18/19") %>% kruskal_test(costs ~ 0 + tri_size)
res.kruskal1920 <- DATA %>% filter(year == "19/20") %>% kruskal_test(costs ~ 0 + tri_size)
res.kruskal1819
res.kruskal1920
# The interpretation values commonly in published literature are: 0.01- < 0.06 (small effect), 0.06 - < 0.14 (moderate effect) and >= 0.14 (large effect).
DATA %>% kruskal_effsize(costs ~ 0 + tri_size)
# https://www.stata-journal.com/article.html?article=st0381
# https://rcompanion.org/rcompanion/d_06.html
pwc1819 <- DATA %>% filter(year == "18/19") %>% rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") 
pwc1920 <- DATA %>% filter(year == "19/20") %>% rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") 
pwc1819
pwc1920
#pwc <- pwc %>% add_xy_position(x = "tri_size") %>% mutate(y.position = c(80, 100, 120))
pwc1819 <- pwc1819 %>% rstatix::add_xy_position(x = "tri_size")
pwc1920 <- pwc1920 %>% rstatix::add_xy_position(x = "tri_size")

p1819 <- DATA %>% filter(costs < 200 & year == "18/19") %>% 
ggplot(.,
       aes(
         y = costs,
         x = tri_size
       )) +
  geom_boxplot() +
  theme_classic() + ylab("Costs [Euro]") + xlab("Operation Size [Number Colonies]") +
  #geom_text(aes(x = 3, y = 12.3, label="italic(p) < 0.05"), color = "gray", parse=T) + 
  #annotate("text", x = 6, y = 10.3, label=paste("mean =", round(mean(DATA$costs),2)), color = "gray") + 
  #geom_hline(yintercept=mean(DATA$costs), color=colorBlindBlack8[2]) +
  theme(
    axis.text.x = element_text(size = 14)
  ) +
  ggtitle("Survey 18/19", subtitle = get_test_label(res.kruskal1819, detailed = TRUE)) +
  labs(caption=str_replace(get_pwc_label(pwc1819, type="text"), "pwc", "Pairwise Comparisons")) +
  #scale_x_discrete(labels = paste0(n_p$tri_size, "\nn=", n_p$participants, "\nhives=", n_p$hives)) +
  
  ggsignif::geom_signif(
    data=pwc1819,
    aes(xmin=xmin, xmax=xmax, annotations=p.adj.signif, y_position = c(80,85,95), group = p.adj),
    textsize = 3, color = "black", manual=TRUE, parse=FALSE)

p1920 <- DATA %>% filter(costs < 200 & year == "19/20") %>% 
  ggplot(.,
         aes(
           y = costs,
           x = tri_size
         )) +
  geom_boxplot() +
  theme_classic() + ylab("Costs [Euro]") + xlab("Operation Size [Number Colonies]") +
  #geom_text(aes(x = 3, y = 12.3, label="italic(p) < 0.05"), color = "gray", parse=T) + 
  #annotate("text", x = 6, y = 10.3, label=paste("mean =", round(mean(DATA$costs),2)), color = "gray") + 
  #geom_hline(yintercept=mean(DATA$costs), color=colorBlindBlack8[2]) +
  theme(
    axis.text.x = element_text(size = 14)
  ) +
  ggtitle("Survey 19/20",subtitle = get_test_label(res.kruskal1920, detailed = TRUE)) +
  labs(caption=str_replace(get_pwc_label(pwc1920, type="text"), "pwc", "Pairwise Comparisons")) +
  #scale_x_discrete(labels = paste0(n_p$tri_size, "\nn=", n_p$participants, "\nhives=", n_p$hives)) +
  
  ggsignif::geom_signif(
    data=pwc1920,
    aes(xmin=xmin, xmax=xmax, annotations=p.adj.signif, y_position = c(80,85,95), group = p.adj),
    textsize = 3, color = "black", manual=TRUE, parse=FALSE)


p1819 | p1920

xk <- kruskal.test(costs ~ tri_size, data=DATA)
confint(xk)
xk
summary(xk)

PT = pairwise.wilcox.test(DATA$costs,
                          DATA$tri_size,
                          p.adjust.method="holm")

confint(PT)


str(DATA2$tri_size)
ggplot(DATA, aes(x = tri_size, y = costs)) + geom_boxplot()

x <- aov(costs ~ 0 + tri_size, data = DATA)
x
confint(x)
summary(x)
y <- broom::tidy(x)
TukeyHSD(x)
costs_size_pairwise <- pairwise.t.test(DATA2$costs, DATA2$tri_size, p.adj = c("holm"))
costs_size_pairwise
broom::tidy(costs_size_pairwise)


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












