data(ctsib, package='faraway')
ctsib$stable <- ifelse(ctsib$CTSIB==1,1,0)
library(geepack)
modgeep <- geeglm(stable ~ Sex + Age + Height + Weight + Surface + Vision,
                  id=Subject, corstr='exchangeable', scale.fix=TRUE,
                  data = ctsib, family=binomial)
summary(modgeep)

modgeep2 <- geeglm(stable ~ Sex + Age + Height + Weight + Surface,
                   id = Subject, corstr = 'exchangeable', scale.fix = TRUE,
                   data=ctsib, family=binomial)
anova(modgeep2, modgeep)

data(epilepsy, package = 'faraway')

modgeep <- geeglm(seizures ~ offset(log(timeadj)) + expind + treat + I(expind*treat), 
                  id=id, family=poisson, corstr = 'ar1',
                  data=epilepsy, subset=(id!=49))
summary(modgeep)

