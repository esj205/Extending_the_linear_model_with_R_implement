data(psid, package = 'faraway')
head(psid)



library(dplyr)
psid20 <- filter(psid, person <=20)
library(ggplot2)
ggplot(psid20, aes(x=year, y=income)) + geom_line() + facet_wrap(~ person)

ggplot(psid20, aes(x=year, y=income+100, group = person)) + 
  geom_line() +
  facet_wrap(~ sex) + 
  scale_y_log10()

lmod <- lm(log(income) ~ I(year-78), subset=(person==1), psid)  
coef(lmod)

library(lme4)
ml <- lmList(log(income) ~ I(year-78) | person, psid)
intercepts <- sapply(ml, coef)[1,]
slopes <- sapply(ml, coef)[2,]

plot(intercepts, slopes, xlab='Intercept', ylab='Slope')
psex <- psid$sex[match(1:85, psid$person)]
boxplot(split(slopes, psex))

t.test(slopes[psex=='M'], slopes[psex=='F'])

t.test(intercepts[psex=='M'], intercepts[psex=='F'])

library(lme4)
psid$cyear <- psid$year - 78
mmod <- lmer(log(income) ~ cyear*sex + age + educ + (cyear|person), psid)

library(faraway)
sumary(mmod, digits=3)

library(pbkrtest)
mmod <- lmer(log(income) ~ cyear*sex + age + educ +(cyear|person), psid, REML=FALSE)
mmodr <- lmer(log(income) ~ cyear + sex + age + educ + (cyear|person), psid, REML = FALSE)
KRmodcomp(mmod, mmodr)

confint(mmod, method = 'boot')

diagd <- fortify.merMod(mmod)
ggplot(diagd, aes(sample=.resid)) + stat_qq() + facet_grid(~sex)

diagd$edulevel <- cut(psid$educ, c(0, 8.5, 12.5, 20), labels=c('lessHS', 'HS', 'moreHS'))
ggplot(diagd, aes(x=.fitted, y=.resid)) +
  geom_point(alpha=0.3) +
  geom_hline(yintercept = 0) +
  facet_grid(~ edulevel) +
  xlab('Fitted') +
  ylab('Residuals')
