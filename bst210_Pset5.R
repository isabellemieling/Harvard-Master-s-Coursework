# Homework 5
# Multinomial and ordinal Logistic Regression with Framingham heart data

library(foreign)
library(nnet)
# set wd and attach relevant dataset

setwd("/Users/isabelle/Desktop/Harvard/Fall2016/BST210/Homework Stuff/Homework5")
fheart = read.dta("framingham.dta")
attach(fheart)

# first, restrict analysis to exclude subjects with prevalent coronary heart disease
# if prevchd = 1, exclude
fheart.sub <- subset(fheart, prevchd == 0)

#create outcome variable
# 1 = no death or coronary heart disease in the follow-up period (reference category)
# 2 = coronary heart disease in the follow-up period, but the subject remained alive
# 3	=  death from any cause in the follow-up period.

fheart.sub$outcome <- ifelse(
  fheart.sub$death == 0, ifelse(fheart.sub$anychd == 0, 1, 2), 3)

#recode indicator variable for female (1=female, 0=male)
#data originally coded as female = 2, male = 1
fheart.sub$female <- ifelse(fheart.sub$sex == 2, 1, 0)

## variables of interest
# participant sex: female
# the outcome category: outcome
# age

# multinomial logistic regressions 
# a function that will help us summarize multinomial models
summ.MNfit <- function(fit, digits=3){
  s <- summary(fit)
  for(i in 2:length(fit$lev))
  {
    ##
    cat("\nLevel", fit$lev[i], "vs. Level", fit$lev[1], "\n")
    ##
    betaHat <- s$coefficients[(i-1),]
    se <- s$standard.errors[(i-1),]
    zStat <- betaHat / se
    pval <- 2 * pnorm(abs(zStat), lower.tail=FALSE)
    ##
    RRR <- exp(betaHat)
    RRR.lo <- exp(betaHat - qnorm(0.975)*se)
    RRR.up <- exp(betaHat + qnorm(0.975)*se)
    ##
    results <- cbind(betaHat, se, pval, RRR, RRR.lo, RRR.up)
    print(round(results, digits=digits))
  }
}
# model 1: age alone
multi.1 = multinom(outcome ~ age, data = fheart.sub)
summary(multi.1)
summ.MNfit(multi.1)

# model 2: sex alone
multi.2 = multinom(outcome ~ female, data=fheart.sub)
summary(multi.2)
summ.MNfit(multi.2)

# model 3: age and sex
multi.3 = multinom(outcome ~ female + age, data=fheart.sub)
summary(multi.3)
summ.MNfit(multi.3)

# model 4: age, sex, and age*sex 
multi.4 = multinom(outcome ~ female + age + female*age, data=fheart.sub)
summary(multi.4)
summ.MNfit(multi.4)




####  ordinal logistic regressions #### 
install.packages("VGAM")
library(VGAM)
# NOTE: This package specifies things differently than the Stata function
# We will get the negative of the coefficients we did in Stata if we set REVERSE = FALSE, which is the default

#model 1: age alone
ord.1 = vglm(outcome ~ age,
             cumulative(parallel=TRUE, reverse=TRUE), data=fheart.sub)
summary(ord.1)

#model 2: sex alone
ord.2 = vglm(outcome ~ female,
             cumulative(parallel=TRUE, reverse=TRUE), data=fheart.sub)
summary(ord.2)


#model 3: age and sex
ord.3 = vglm(outcome ~ age + female,
             cumulative(parallel=TRUE, reverse=TRUE), data=fheart.sub)
summary(ord.3)

#model 4: age, sex, interaction
ord.4 = vglm(outcome ~ age + female + age*female,
             cumulative(parallel=TRUE, reverse=TRUE), data=fheart.sub)
summary(ord.4)

# question 2b: for the model with age alone, is the proportional odds satisfied or rejected?
fheart.sub$b1 <- ifelse(fheart.sub$outcome == 3, 1, 0)
# run a logistic model using age to predict this binary outcome (b1)
b1.glm <- glm(b1 ~ age, family=binomial(), data=fheart.sub)
summary(b1.glm)

fheart.sub$b2 <- ifelse(fheart.sub$outcome == 1, 0, 1)
# run a logistic model using age to predict b2
b2.glm <- glm(b2 ~ age, family=binomial(), data=fheart.sub)
summary(b2.glm)

b3.glm <- glm(b2 ~ age + age*sex, family=binomial(), data=fheart.sub)
summary(b3.glm)

# Performing the Likelihood Ratio test 
#(reduced, full)
anova(ord.3, ord.4, test="Chisq")


### 1b ###
# Look at how fitted values compare to observed proportions
table(fheart.sub$outcome, fheart.sub$female)
prop.table(table(fheart.sub$outcome, fheart.sub$female), margin = 2)
# Female fitted probs.
fitted(multi.2)[fheart.sub$female == 1,][1,]
# Male fitted probs.
fitted(multi.2)[fheart.sub$female == 0,][1,]


