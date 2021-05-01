###########################
##Setup
data(gavote, package='faraway')
library(faraway)
help(gavote)
help(quantile)
help.search('quantiles')
############################
##데이터 살펴보기
gavote
head(gavote)
str(gavote)
summary(gavote)

############################
##Undercount data
gavote$undercount <- (gavote$ballots-gavote$votes)/gavote$ballots
summary(gavote$undercount)
with(gavote, sum(ballots-votes)/sum(ballots))
hist(gavote$undercount, main='Undercount', xlab='Percent Undercount')
plot(density(gavote$undercount), main="Undercount")
rug(gavote$undercount)


############################
##categorical data visualization
pie(table(gavote$equip), col=gray(0:4/4))
barplot(sort(table(gavote$equip), decreasing = TRUE), las=2) ##las=2 bar label들이 세로로 나타나게 함.

############################
##Scatter Plot
gavote$pergore <- gavote$gore/gavote$votes
plot(pergore ~ perAA, gavote, xlab='Proportion African American', ylab='Proportion for Gore')

############################
##Side-by-side boxplots
plot(undercount ~ equip, gavote, xlab='', las=3)


############################
##Cross-tabulations
xtabs(~ atlanta + rural, gavote)

############################
##Change the variable name
names(gavote)
names(gavote)[4] <- 'usage' ## 변수 내의 level의 label이랑 이름이 중복돼서 혼란을 피하기 위해 바꿔준다.

############################
##Correlation
nix <- c(3,10,11,12)
cor(gavote[,nix])


############################
###Linear Model############
###########################

############################
##Fitting a Linear Model
lmod <- lm(undercount~pergore+perAA, gavote)
coef(lmod)

predict(lmod)
residuals(lmod)

deviance(lmod)  
##linear model에서 residual sum of squares(RSS)를 뱉어냄. 
##more general measure of fit than RSS

df.residual(lmod)
#number of cases - the number of coefficients
nrow(gavote) - length(coef(lmod))

##Estimated Variance of the error 
sqrt(deviance(lmod)/df.residual(lmod))
lmodsum <- summary(lmod)
lmodsum$sigma

##coefficient of determination or percentage of variance explained or R^2
lmodsum$r.squared
cor(predict(lmod), gavote$undercount)^2

##Adjusted R^2
lmodsum$adj.r.squared


##summary
summary(lmod)
library(faraway)
sumary(lmod)


############################
##Qualitative Predictors

##Center the variables for being clear
gavote$cpergore <- gavote$pergore - mean(gavote$pergore)
gavote$cperAA <- gavote$perAA - mean(gavote$perAA)

##add some qualitative variables (usage, equip)


lmodi <- lm(undercount ~ cperAA+cpergore*usage+equip, gavote)

summary(lmodi)


############################
##Hypothesis Testing

anova(lmod, lmodi) #p-value가 0.05보다 낮으므로 Model 1을 기각한다. 즉, Model 2 채택
drop1(lmodi, test='F')

############################
##Confidence Interval
confint(lmodi)


############################
##Diagnostics 
###########################
plot(lmodi)

gavote[cooks.distance(lmodi)>0.1,]

halfnorm(hatvalues(lmodi))
gavote[hatvalues(lmodi)>0.3,]

termplot(lmodi, partial=TRUE, terms=1)


############################
##Robust Regression 
###########################
library(MASS)
rlmodi<- rlm(undercount ~ cperAA+cpergore*usage+equip, gavote)
summary(rlmodi)

############################
##Weighted Least Squares 
###########################
wlmodi <- lm(undercount ~ cperAA+cpergore*usage+equip, gavote, weights=ballots)
summary(wlmodi)

min(predict(wlmodi))

############################
##Transformation
###########################
plmodi <- lm(undercount ~ poly(cperAA, 4) + cpergore*usage + equip, gavote)
summary(plmodi)

termplot(plmodi, partial=TRUE, terms=1)

##splines
library(splines)
blmodi <- lm(undercount ~ cperAA + bs(cpergore, 4) + usage + equip, gavote)
termplot(blmodi, partial=TRUE, terms=2)

############################
##Variable Selection
###########################
biglm <- lm(undercount ~ (equip+econ+usage+atlanta)^2 + (equip+econ+usage+atlanta)*(perAA+pergore), gavote)
smallm <- step(biglm, trace=FALSE)
summary(smallm)

drop1(smallm, test='F')

finalm <- lm(undercount ~ equip + econ + perAA + equip*econ + equip:perAA, gavote)
sumary(finalm)

############################
##conclusion
###########################
pdf <- data.frame(econ=rep(levels(gavote$econ), 5), equip=rep(levels(gavote$equip), rep(3,5)), perAA=0.233)
pdf
pp <- predict(finalm, new=pdf)
xtabs(round(pp,3) ~ econ + equip, pdf)

pdf <- data.frame(econ=rep('middle', 15), 
                  equip = rep(levels(gavote$equip), rep(3,5)),
                  perAA=rep(c(.11, 0.23, 0.35), 5))
pp <- predict(finalm, new=pdf)

propAA <- gl(3,1,15, labels=c('low', 'medium', 'high'))
xtabs(round(pp, 3) ~ propAA + equip, pdf)

sumary(lmodi)

sqrt(0.035*(1-0.035)/881)

min(gavote$ballots)
