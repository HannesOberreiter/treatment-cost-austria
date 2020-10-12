t <- DATA %>% group_by(t_short_number) %>% summarise(
  c_mean = round(mean(costs),2),
  e_mean = round(mean(t_estimated),2),
  n = n()
)


ggplot(t[t$c_mean<20,]) + 
  aes(x=c_mean, y=c_mean, color=t_short_number) + 
  geom_point(show.legend = F)


paste0(treatmentList$ttotal[2],12)

d <- DATA %>% group_by(t_short) %>% summarise(
  
  `AS-KZ`  = sum(T_formic_short_total12),
  `AS_KZ  = sum(T_formic_short_total12),
  
  c_mean = round(mean(costs),2),
  e_mean = round(mean(t_estimated),2),
  n = n(),
  m_lost = mean((hives_lost*100/hives_winter)),
  s_lost = sd((hives_lost*100/hives_winter))
) %>% filter(n>50)


ggplot(d) + aes(y = c_mean, x = m_lost) +
  geom_errorbar(aes(ymax = m_lost + s_lost, ymin = m_lost - s_lost)) +
  geom_point(aes(size=n)) +
  geom_vline(aes(xintercept = mean(d$m_lost), color = "red"))


#%>% filter(str_detect(t_short, "AS-KZ"))

ggplot(t[t$c_mean<20,]) + 
  aes(x=c_mean, y=c_mean, color=t_short_number) + 
  geom_point(show.legend = F)




DATA$new <- str_replace_all(DATA$t_short, "Drohne & ", "")

d <- DATA %>% group_by(new) %>% summarise(
  c_mean = mean(costs),
  e_mean = mean(t_estimated),
  
  
  n = n()
)


ggplot(d) +
  aes(x = c_mean, y = e_mean) +
  geom_point()

ggplot(DATA) +
  aes(x = costs, y = (hives_lost*100/hives_winter)) +
  geom_abline(intercept = 0, slope = 1) +  
  geom_point(show.legend=F)

ggplot(DATA) +
  
  geom_violin(aes(x = "Survey", y = costs), outlier.shape = NA) +
  geom_violin(aes(x = "Estimates", y = t_estimated), outlier.shape = NA)+
  geom_boxplot(aes(x = "Survey", y = costs), outlier.shape = NA) +
  geom_boxplot(aes(x = "Estimates", y = t_estimated), outlier.shape = NA)+
  
  ggtitle("", subtitle = "* Outliers Hidden") + xlab("") + ylab("Cost/Colony [Euro]") +
  theme_classic() + 
  ylim(0,25)

my.formula = y ~ x
ggplot(DATA[DATA$hives_winter < 100,]) +
  aes(x = hives_winter, y = costs) +
  geom_smooth(formula = my.formula, method = "lm", se = TRUE) + 
  stat_poly_eq(formula = my.formula, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +     
  geom_point(show.legend=F)



DATA$t_desc_od <- str_replace(DATA$t_desc, "Drohnenbrutentnahme & ", "")
DATA$t_short_od <- str_replace(DATA$t_short, "Drohne & ", "")


# Bland-Altmann Plot
d <- DATA %>% 
  group_by(t_short_od) %>% summarise(
    n = n(),
    p = round(n()*100/nrow(DATA),2),
    mc = round(mean(costs), 2),
    medc = round(median(costs), 2),
    me = round(mean(t_estimated), 2),
    mede = round(median(t_estimated), 2),
    
  ) %>% filter(n >= 15) %>% arrange(desc(n))


dif <- d$mc - d$me
m <- apply(d[,c(4,6)], 1, mean)
# mean and sd from difference
dif_mean <- mean(dif)
dif_sd   <- sd(dif)
# agreement borders
upper    <- dif_mean + 2 * dif_sd
lower    <- dif_mean - 2 * dif_sd

limit_y = round( ifelse( max(dif) > (-1*min(dif)), max(dif), -1*min(dif) ) + 0.5, digits = 1)

ggplot() +
  aes(x = m, y = dif) +
  geom_point()+
  ylim(c( -1*limit_y, limit_y ))



  

c(rep(colorBlindBlack8[1], 2), rep(colorBlindBlack8[2], 2), rep(colorBlindBlack8[3], 2))



pl


plot(
  x    = m, 
  y    = d,
  type = "n",
  xlab = "Mean [L/min]",
  ylab = "Difference [L/min]",
  main = "Bland-Altman - cardiac output measurement methods",
  xlim = c( 0, max(m)+1 ),
  ylim = c( -1*limit_y, limit_y )
)
grid() # grid lines
abline(h = d_mean, col = "red", lwd = 2) # mean difference line
abline(h = upper, col = "red", lty = 2)  # upper CI
abline(h = lower, col = "red", lty = 2)  # lower CI
points(m, d) # points later to print them over the lines
print("Bland-Altmann Plot created.")


d[,c(4,6)]
FUNC_bland_altmann <- function(x){
  d        <- x$method_1 - x$method_2    # difference vector, y axis
  m        <- mean()


lin_reg = y ~ x
lin_reg = lm(me ~ mc, d)
print(lin_reg)
summary(lin_reg)
ggplot(
  d, aes(x=mc, y=me, color=t_short_od, size = p)
  ) +
  geom_point(show.legend = F) +
  geom_abline(slope=15) +
  #geom_smooth(method = "lm", se = TRUE, show.legend = F) + 
  theme_classic()

blandr.display( d$mc , d$me , sig.level=0.95 )
x<-blandr.statistics(d$mc , d$me , sig.level=0.95)
x<-blandr.statistics(DATA$costs , DATA$t_estimated , sig.level=0.95)
blandr.plot.ggplot(x)
# t - test? 

