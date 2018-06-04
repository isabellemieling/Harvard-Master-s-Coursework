# BST 210 - Homework 6 - November 15, 2016

setwd("/Users/isabelle/Desktop/Harvard/Fall2016/BST210/Homework Stuff/Homework6")
getwd()

library(foreign)
library(survival)
library(MASS)
library(stats)

###################################
# Conditional Logistic Regression #
###################################
bbd = read.table("Benign breast disease data.txt", skip=14, col.names=c("row", "stratum", "observation", "age", "checkups", "age_preg", "age_menarche", "weight"))
attach(bbd)

# re-code checkups so that 1= Yes and 0=No
# in raw data, 1= Yes and 2= No --> so 2-No-0, Yes-1
bbd$checkups <- ifelse(bbd$checkups==2, 0, 1)

#1(a)	First perform a 1:1 matched analysis using the case (observation = 1) and first control (observation = 2) 
#to assess whether or not having regular medical checkups was a risk factor for benign breast disease

# only need observation = 1 and observation = 2 --> drop 3,4 
bbd_1a <- bbd[(bbd$observation == 1) | (bbd$observation == 2),]

# create case control indicator 
# 1 = case, 0 = control ; observation2=control=0, case=1
bbd_1a$caco <- ifelse(bbd_1a$observation == 2, 0, 1)

# fit conditional logistic regression
fit1a <- clogit(caco ~ checkups + strata(stratum), data=bbd_1a)
summary(fit1a)
# we see from the summary that the coefficient for checkups is statistically significnat (p<0.005) 
# therefore, having or not having regular medical checkups is a risk factor for benign breast disease becuase those cases with breast disease are 4.33 times mroe likely to have medical checkusp 
# this is conditioned on being in the same matched set
# We estimate that individuals with benign breast disease (cases) have 4.33 times the odds of having regular medical checkups holding all other factors constant, includin being within the same matched set 

#1(b) Perform 1:3 matched set using all observatiions of controls
#to assess whether or not having regular medical checkups was a risk factor for benign breast disease

# create case control indicator ; observation1=case=1, control=0
bbd$caco <- ifelse(bbd$observation == 1, 1, 0)

# fit conditional logistic regression
fit1b <- clogit(caco ~ checkups + strata(stratum), data=bbd)
summary(fit1b)

fit1b_age <- clogit(caco ~ checkups + age + strata(stratum), data=bbd)
summary(fit1b_age)
# we see that adding age as a covariate in the model does not help the conditional logistic regression analysis
# the coefficient for checkups remains the same and is still statistically significant (p<0.005). 
# adding age of the subject at matching as a covariate does not help our conditional logistic regression analysis becasue cases and controls are already matched on age and this analysis is done within a matching set  

#1(c)# conditional logistic regression analysis using integer age to match cases and controls
# rather than using the 1:3 matching

fit1c <- clogit(caco ~ checkups + strata(age), data=bbd)
summary(fit1c)
# Here we see that in doing this we get only a slightly different odds ratio for checkups and it is statistically significant (p<0.005)

# how many strata? 30
length(unique(bbd$age))
# size of largest stratum? 20
table(bbd$age)

#1(d) Appropriate matched analysis to build a model looking at effects of all possible covariates 
# Be sure to carefully think about the age at first pregnancy variable, as it is not really missing for anyone. 

# If missing age at first pregnancy, set = 0 
bbd$age_preg <- as.character(bbd$age_preg)
bbd$age_preg[bbd$age_preg == '.'] <- 0
bbd$age_preg <- as.numeric(bbd$age_preg)

# model with all covariates
fit_1d <- clogit(caco ~ checkups + age_preg + age_menarche + weight + strata(stratum), data=bbd)
summary(fit_1d)



###################################
# Question 2 #
###################################

lungcancer = read.table("fromlungcancer.txt", skip = 11, col.names=c("SMOKEDUR", "CIGPDAY", "CASES", "MANYEARS"))
attach(lungcancer)

#2a Effect of cigpday on lung cancer incidence. linear smokedur confounder or effect modifier on effects of linear cigpday on lung cancer incidence ? 
# confounder: with and without 
lung1 <- glm(CASES ~ CIGPDAY, offset = log(MANYEARS), family=poisson(), data=lungcancer)
summary(lung1)

# IRR 
exp(coef(lung1)[2])
confint(lung1)

#fit model including smokedur: confounder?
lung2 <- glm(CASES ~ CIGPDAY + SMOKEDUR, offset = log(MANYEARS), data=lungcancer, family=poisson())
summary(lung2)

## more analysis of model including smokedur 
# IRR
exp(coef(lung2)[2])
confint(lung2)

#is smokedur an effect modifier?
lung3 <- glm(CASES ~ CIGPDAY*SMOKEDUR, offset=log(MANYEARS), data=lungcancer, family=poisson())
summary(lung3)
anova(lung2, lung3, test="Chisq")

# 2C
# Consider a model including linear and quadratic effects of both CIGPDAY and SMOKEDUR.
CIGPDAY.2 <- CIGPDAY^2
SMOKEDUR.2 <- SMOKEDUR^2
lung4 <- glm(CASES ~ CIGPDAY + CIGPDAY.2 + SMOKEDUR + SMOKEDUR.2, offset=log(MANYEARS), data=lungcancer, family=poisson())
summary(lung4)
# point estimate for IRR 20 cigs p day vs 0
exp(20*coef(lung4)[2]+20^2*coef(lung4)[3])
# point estimate for IRR 40 cigs p day vs 20
exp((40-20)*coef(lung4)[2]+(40^2-20^2)*coef(lung4)[3])
# confidence intervals
confint(lung4)


?#Does this model show improvements relative to the model including only linear covariates?
anova(lung2, lung4, test='Chisq')

# 2D 
# In the first interaction model, just add the CIGPDAY*SMOKEDUR interaction term (one parameter). 
# In the second interaction model, add in interactions between the linear and quadratic effects of CIGPDAY and SMOKEDUR 
# (so four interaction parameters needed).

lung5 <- glm(CASES ~ CIGPDAY + CIGPDAY.2 + SMOKEDUR + SMOKEDUR.2 + CIGPDAY*SMOKEDUR, offset=log(MANYEARS), data=lungcancer, family=poisson() )
summary(lung5)
# LLR
anova(lung4, lung5, test='Chisq')

lung6 <- glm(CASES ~ CIGPDAY + CIGPDAY.2 + SMOKEDUR + SMOKEDUR.2 + CIGPDAY*SMOKEDUR + CIGPDAY*SMOKEDUR.2 + CIGPDAY.2*SMOKEDUR + CIGPDAY.2*SMOKEDUR.2, offset=log(MANYEARS), data=lungcancer, family=poisson() )
summary(lung6)
#LLR
anova(lung4, lung6, test='Chisq')

# 2E 
# Categorical CIGSPDAY and SMOKEDUR
# create categorical variable for CIGSPDAY
lungcancer$CIGSPDAY.CAT = 1
lungcancer$CIGSPDAY.CAT = lungcancer$CIGSPDAY.CAT + (lungcancer$CIGPDAY > 10) + (lungcancer$CIGPDAY > 20) + (lungcancer$CIGPDAY > 30)

# create categorical variable for SMOKEDUR
lungcancer$SMOKEDUR.CAT = 1
lungcancer$SMOKEDUR.CAT = lungcancer$SMOKEDUR.CAT + (lungcancer$SMOKEDUR > 27.5) + (lungcancer$SMOKEDUR> 37.5) + (lungcancer$SMOKEDUR > 47.5)

# fit model with categorical CIGSPDAY and SMOKEDUR
lung7 <- glm(CASES ~ as.factor(CIGSPDAY.CAT) + as.factor(SMOKEDUR.CAT), offset=log(MANYEARS), data=lungcancer, family=poisson())
summary(lung7)
confint(lung7)

# 2F
# Consider whether one of the extensions to Poisson regression modeling would be helpful with this data analysis. 
# Choosing either (your choice, pick one) quadratic or categorical effects of CIGPDAY and SMOKEDUR in a model (with no interaction terms)
# suggest whether or not you feel your new model is helpful.

# There are a lot of 0's, so we should fit a ZIP
hist(CASES, breaks=seq(0, max(CASES), by=1), col='lightpink')
# Zero-inflated Poisson with constant probability of structural zero

install.packages("pscl")
library(pscl)

lung8 <- zeroinfl(CASES ~ CIGPDAY + CIGPDAY.2 + SMOKEDUR + SMOKEDUR.2 | 1, data=lungcancer, offset= log(MANYEARS))
summary(lung8)

lung9 <- zeroinfl(CASES ~ as.factor(CIGSPDAY.CAT) + as.factor(SMOKEDUR.CAT) | 1, data=lungcancer, offset=log(MANYEARS))
summary(lung9)
confint(lung9)

### 2G
# Compare AIC's
# lung1, lung2, lung4, lung6, lung7, lung8, lung9
AIC(lung1)
AIC(lung2)
AIC(lung4)
AIC(lung6)
AIC(lung7)
AIC(lung8)
AIC(lung9)


