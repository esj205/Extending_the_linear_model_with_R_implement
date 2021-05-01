library(ggplot2)
library(faraway)
library(lme4)
library(pbkrtest)

data(vision, package='faraway')
vision$npower <- rep(1:4, 14)
ggplot(vision, aes(y=acuity, x=npower, linetype=eye)) +
  geom_line() +
  facet_wrap(~ subject, ncol=4) +
  scale_x_continuous('Power', breaks = 1:4, label=c('6/6', '6/18', '6/36', '6/60'))


mmod <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), vision)

sumary(mmod)

4.64^2/(4.64^2+3.21^2+4.07^2)

(4.64^2+3.21^2)/(4.64^2+3.21^2+4.07^2)

mmod <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), vision, REML = FALSE)
nmod <- lmer(acuity ~ 1 + (1|subject) + (1|subject:eye), vision, REML=FALSE)
KRmodcomp(mmod, nmod)

mmodr <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), vision, REML=FALSE, subset=-43)
nmodr <- lmer(acuity ~ 1 + (1|subject) + (1|subject:eye), vision, REML=FALSE, subset = -43)
KRmodcomp(mmodr, nmodr)

op <- options(contrasts = c('contr.helmert', 'contr.poly'))
mmodr <- lmer(acuity ~ power + (1|subject) + (1|subject:eye), vision, subset=-43)

sumary(mmodr)

options(op)
contr.helmert(4)

plot(resid(mmodr) ~fitted(mmodr), xlab='Fitted', ylab='Residuals')
abline(h=0)
qqnorm(ranef(mmodr)$'subject:eye'[[1]], main='')
