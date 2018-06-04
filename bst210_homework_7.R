# Homework 7 - October 18, 2016
# focus on mortality over 24 year period 
# continuous BMI, sex, age as independent variables

getwd()
setwd("/Users/isabelle/Desktop/Harvard/Fall2016/BST210/Homework Stuff/Homework4")

# Loading Packages
library(foreign)
library(ggplot2)

# Reading in the Framingham data
framingham <- read.dta(file="framingham.dta")
framingham <- framingham[complete.cases(framingham), ]

# 1a. Fitting logistic regression model with continuous bmi
model1a.glm <- glm(death ~ bmi, family=binomial(), data=framingham)
summary(model1a.glm)

#1b. Logistic model with linear and quadratic logit bmi 
bmi2 <- (framingham$bmi)^2
model1b.glm <- glm(death ~ bmi + bmi2, family=binomial(), data=framingham)
summary(model1b.glm)

# Performing the Likelihood Ratio test 
#(reduced, full)
anova(model1a.glm, model1b.glm, test="Chisq")

# Part c 
# scatter plot 
plot(framingham$bmi, framingham$sex)

# box plot of bmi by sex
boxplot(framingham$bmi~ factor(framingham$sex, levels = c(1,2),labels = c("Female", "Male")), na.rm = TRUE, xlab = "Sex", ylab = "BMI")

#----------------------------------
# Confounding
#----------------------------------
# comparing linear model to that with sex 

# Fitting a logistic regression model with age
model1c.glm <- glm(death ~ bmi + sex, family=binomial(), data=framingham)
summary(model1c.glm)

# Effect Modification
model1c2.glm <- glm(death ~ bmi*sex, family=binomial(), data=framingham)
summary(model1c2.glm)


# 1d age 

# Creating categorical age
framingham$age.cat <- rep(NA, nrow(framingham))
for (i in 1:nrow(framingham)){
  if (framingham$age[i] <= 45) {
    framingham$age.cat[i] <- 0
  } else if (framingham$age[i] <= 55) {
    framingham$age.cat[i] <- 1
  } 
  else{
    framingham$age.cat[i] <- 2
  }
}

# Fitting a logistic regression model with categorical age
age.category <- glm(death ~ as.factor(age.cat), family=binomial(), data=framingham)
summary(age.category)

# Fitting a logistic regression model with categorical age (treated as continuous)
age.category2 <- glm(death ~ age.cat, family=binomial(), data=framingham)
summary(age.category2)

# Conducting the Likelihood Ratio test (comparing categorical models)
anova(age.category2, age.category, test="Chisq")

#----------------------------------
# Continuous Age

# Fitting a logistic regression model with continuous age
age.continuous <- glm(death ~ age, family=binomial(), data=framingham)
summary(age.continuous)



#----------------------------------
# Confounding
#----------------------------------
# comparing linear model to that with sex 

# Fitting a logistic regression model with age
model1d.glm <- glm(death ~ bmi + age.cat, family=binomial(), data=framingham)
summary(model1d.glm)

# Effect Modification
model1d2.glm <- glm(death ~ bmi*age.cat, family=binomial(), data=framingham)
summary(model1d2.glm)





# Graph fitted probabilities for both models on same plot
curve(exp(coef(model1a.glm)[1] + coef(model1a.glm)[3]*x)/(1+ exp(coef(model1a.glm)[1] + coef(model1a.glm)[3]*x)), xlim=c(0, 100), ylim=c(0,1), xlab="Age", ylab="Risk of Fracture", col="dodgerblue")



# Plotting fitted probabilities
curve(exp(coef(priorfrac3)[1] + coef(priorfrac3)[3]*x)/(1+ exp(coef(priorfrac3)[1] + coef(priorfrac3)[3]*x)), xlim=c(55, 90), ylim=c(0,1), xlab="Age", ylab="Risk of Fracture", col="dodgerblue")
curve(exp(coef(priorfrac3)[1] + coef(priorfrac3)[2] + (coef(priorfrac3)[3] + coef(priorfrac3)[4])*x)/(1 + exp(coef(priorfrac3)[1] + coef(priorfrac3)[2] + (coef(priorfrac3)[3] + coef(priorfrac3)[4])*x)), xlim=c(55, 90), col="magenta", add=T)

(model1a.glm)[1]
(model1a.glm)[2]
(model1a.glm)[3]

plot(framingham$death ~ framingham$bmi, xlab="BMI", ylab="Risk of Death")
lines(fitted(priorfrac3)[order(framingham$bmi)][which(framingham$priorfrac[order(glow$age)]==0)] ~ sort(glow$age)[which(glow$priorfrac[order(glow$age)]==0)], col="pink")
lines(fitted(priorfrac3)[order(framingham$bmi)][which(framingham$priorfrac[order(glow$age)]==1)] ~ sort(glow$age)[which(glow$priorfrac[order(glow$age)]==1)], col="green")

