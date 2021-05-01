library(faraway)

#1. Binomial Regression Model
data(orings, package = 'faraway')
plot(damage/6 ~ temp, orings, xlim=c(25,85), ylim=c(0,1), xlab='Temperature',
     ylab='Prob of damage')

lmod <- glm(cbind(damage, 6-damage) ~ temp, family=binomial, orings)
sumary(lmod)

x <- seq(25,85,1)
lines(x, ilogit(11.6630-0.2162*x))

ilogit(11.6630-0.2162*31)

#2. Inference
pchisq(deviance(lmod), df.residual(lmod), lower=FALSE)

pchisq(38.9, 22, lower=FALSE)

pchisq(38.9-16.9,1,lower=FALSE)

erings <- with(orings, 
               data.frame(temp=rep(temp, each=6),
                          damage=as.vector(sapply(orings$damage, function(x) rep(c(0,1), times=c(6-x,x))))))
head(erings)

emod <- glm(damage ~ temp, family=binomial, erings)
sumary(emod)

confint(lmod)

#3. Pearson's Chisquare Statistic
deviance(lmod)
1-pchisq(28.067, 21)

#4. Overdispersion
data(troutegg, package='faraway')
ftable(xtabs(cbind(survive, total) ~ location+period, troutegg))

bmod <- glm(cbind(survive, total-survive) ~ location+period, family=binomial, troutegg)
sumary(bmod)

halfnorm(residuals(bmod))

elogits <- with(troutegg, log((survive+0.5)/(total-survive+0.5)))
with(troutegg, interaction.plot(period, location, elogits))

(sigma2 <- sum(residuals(bmod, type='pearson')^2)/12)

drop1(bmod, scale=sigma2, test='F')

sumary(bmod, dispersion=sigma2)

library(dispmod)
dmod <- glm.binomial.disp(bmod)
sumary(dmod)

#5. Quasi-Binomial
data(mammalsleep, package='faraway')
mammalsleep$pdr <- with(mammalsleep, dream/sleep)
summary(mammalsleep$pdr)

mod1 <- glm(pdr ~ log(body) + log(brain) + log(lifespan) + 
              log(gestation) + predation + exposure + danger,
            family=quasibinomial, mammalsleep) 

drop1(mod1, test='F')

mod1 <- glm(pdr ~ log(body) + log(lifespan) + danger, family=quasibinomial, mammalsleep)
sumary(mod1)

l1 <- row.names(na.omit(mammalsleep[,c(1,6,10,11)]))
halfnorm(cooks.distance(mod1), labs=l1)
plot(predict(mod1), residuals(mod1, type='pearson'), xlab='Linear Predictor', ylab='Pearson Residuals')

#6. Beta Regression
data(mammalsleep, package='faraway')
mammalsleep$pdr <- with(mammalsleep, dream/sleep)
library(mgcv)
modb <- gam(pdr ~ log(body) + log(lifespan), family=betar(), mammalsleep)
summary(modb)
