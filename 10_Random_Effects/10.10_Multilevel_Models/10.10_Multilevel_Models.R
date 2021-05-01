data(jsp, package = 'faraway')
jspr <- jsp[jsp$year==2,]

library(ggplot2)
ggplot(jspr, aes(x=raven, y=math)) + 
  xlab('Raven Score') +
  ylab('Math Score') +
  geom_point(position=position_jitter(), alpha=0.3)
ggplot(jspr, aes(x=social, y=math)) +
  xlab('Social Class') +
  ylab('Math Score') +
  geom_boxplot()

glin <- lm(math ~ raven*gender*social, jspr)
anova(glin)

glin <- lm(math ~ raven*social, jspr)
anova(glin)

glin <- lm(math ~ raven + social, jspr)
summary(glin)

table(jspr$school)

library(lme4)
#lmer --> Linear Mixed Model fitting
mmod <- lmer(math ~ raven*social*gender+(1|school)+(1|school:class), data=jspr)

library(pbkrtest)
mmodr <- lmer(math ~ raven*social+(1|school)+(1|school:class), data=jspr)
KRmodcomp(mmod, mmodr)

all3 <- lmer(math ~ raven*social*gender + (1|school) + (1|school:class),
             data=jspr, REML = FALSE)
all2 <- update(all3, . ~ . - raven:social:gender)
notrs <- update(all2, . ~ . - raven:social)
notrg <- update(all2, . ~ . - raven:gender)
notsg <- update(all2, . ~ . - social:gender)
onlyrs <- update(all2, . ~ . - social:gender - raven:gender)
all1 <- update(all2, . ~ . - social:gender - raven:gender - social:raven)
nogen <- update(all1, . ~ . -gender)

anova(all3, all2, notrs, notrg, notsg, onlyrs, all1, nogen)[,1:4]

library(faraway)
jspr$craven <- jspr$raven - mean(jspr$raven)
mmod <- lmer(math ~ craven*social+(1|school)+(1|school:class), jspr)
sumary(mmod)

diagd <- fortify(mmod)
ggplot(diagd, aes(sample=.resid))+stat_qq()
ggplot(diagd, aes(x=.fitted, y=.resid)) + geom_point(alpha=0.3) +
  geom_hline(yintercept = 0) + xlab('Fitted') + ylab('Residuals')

qqnorm(ranef(mmod)$school[[1]], main='School effects')
qqnorm(ranef(mmod)$'school:class'[[1]], main = 'Class effects')

adjscores <- ranef(mmod)$school[[1]]

rawscores <- coef(lm(math ~ school-1, jspr))
rawscores <- rawscores - mean(rawscores)

plot(rawscores, adjscores)
sint <- c(9, 14, 29)
text(rawscores[sint], adjscores[sint]+0.2, c('9', '15', '30'))

library(RLRsim)
mmodc <- lmer(math ~ craven*social+(1|school:class), jspr)
mmods <- lmer(math ~ craven*social+(1|school), jspr)

exactRLRT(mmodc, mmod, mmods)

exactRLRT(mmods, mmod, mmodc)

schraven <- lm(raven ~ school, jspr)$fit

mmodc <- lmer(math ~ craven*social+schraven*social+(1|school)+(1|school:class), jspr)
KRmodcomp(mmod, mmodc)
