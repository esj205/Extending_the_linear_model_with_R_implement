library(faraway)
data(ctsib, package='faraway')
ctsib$stable <- ifelse(ctsib$CTSIB==1, 1, 0)

xtabs(stable ~ Surface + Vision, ctsib)/80

library(dplyr)
subsum <- ctsib %>% 
  group_by(Subject) %>% 
  summarise(Height=Height[1], Weight = Weight[1], stable = mean(stable), Age = Age[1], Sex = Sex[1])
library(ggplot2)
ggplot(subsum, aes(x=Height, y=stable)) + geom_point()
ggplot(subsum, aes(x=Weight, y=stable)) + geom_point()
ggplot(subsum, aes(x=Age, y=stable)) + geom_point()
ggplot(subsum, aes(x=Sex, y=stable)) + geom_boxplot()

gf <- glm(stable ~ Sex + Age + Height + Weight + Surface + Vision, binomial, data = ctsib)
sumary(gf)

gfs <- glm(stable ~ Sex + Age + Height + Weight + Surface + Vision + factor(Subject),
           binomial, data = ctsib)

library(MASS)
modpql <- glmmPQL(stable ~ Sex + Age + Height + Weight + Surface + Vision,
                  random = ~1|Subject, family = binomial, data = ctsib)
summary(modpql)

library(lme4)
modlap <- glmer(stable ~ Sex + Age + Height + Weight + Surface + Vision + (1|Subject),
                family = binomial, data = ctsib)

modgh <- glmer(stable ~ Sex + Age + Height + Weight + Surface + Vision + (1|Subject),
               nAGQ = 25, family = binomial, data = ctsib)

summary(modgh)

modgh2 <- glmer(stable ~ Surface + Vision + (1|Subject), nAGQ = 25,
                family = binomial, data = ctsib)
anova(modgh, modgh2)

dd <- fortify.merMod(modgh2)

ggplot(dd, aes(sample=.resid)) + stat_qq() + facet_grid(Surface ~ Vision)


library(INLA)
formula <- stable ~ Surface + Vision + f(Subject, model='iid')
result <- inla(formula, family = 'binomial', data = ctsib)

sigmaalpha <- inla.tmarginal(function(x){1/sqrt(x)}, result$marginals.hyperpar$`Precision for Subject`)

x <- seq(0, 7, length.out = 100)
sdf <- data.frame(yield = x, density = inla.dmarginal(x, sigmaalpha))
ggplot(sdf, aes(x=yield, y=density)) + geom_line()

restab <- sapply(result$marginals.fixed, function(x){inla.zmarginal(x, silent=TRUE)})
restab <- cbind(restab, inla.zmarginal(sigmaalpha, silent=TRUE))
colnames(restab) = c('mu', 'norm', 'dome', 'open', 'alpha')
data.frame(restab)

x <- seq(-2, 11, length.out = 100)
rden <- sapply(result$marginals.fixed, function(y){inla.dmarginal(x, y)})[,-1]
ddf <- data.frame(yield = rep(x,3), density = as.vector(rden), 
                  treat = gl(3, 100, labels = c('norm', 'dome', 'open')))
ggplot(ddf, aes(x=yield, y=density, linetype = treat)) + geom_line()

2*inla.pmarginal(0, result$marginals.fixed$Visiondome)

xm <- model.matrix(~ Sex + Age + Height + Weight + Surface + Vision, ctsib)
stabledat <- with(ctsib, list(Nobs = nrow(ctsib),
                              Nsubs = length(unique(ctsib$Subject)),
                              Npreds = ncol(xm),
                              y=stable,
                              subject = Subject,
                              x = xm))

library(rstan)
rt <- stanc('glmmbin.stan')
sm <- stan_model(stanc_ret = rt, verbose = FALSE)
fit <- sampling(sm, data=stabledat)

traceplot(fit, pars = 'sigmasubj', inc_warmup=FALSE)

print(fit, pars=c('sigmasubj', 'beta'))


ipars <- data.frame(extract(fit, pars=c('sigmasubj', 'beta')))
colnames(ipars)[-1] <- colnames(xm)
library(reshape2)
rdf <- melt(ipars)
ggplot(rdf, aes(x=value)) + 
  geom_density() + 
  facet_wrap(~ variable, scales='free') +
  geom_vline(xintercept = 0)

ppars <- data.frame(extract(fit, pars='subeff'))  
sort(colMeans(ppars))
