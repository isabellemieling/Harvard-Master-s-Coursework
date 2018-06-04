# Question 1: explore effects of age to predict tc 
# a. run 3 linear regression models using linear age, linear and quadratic age, spline of GAM modeling of age

setwd("/Users/isabelle/Desktop/Harvard/Fall2016/BST210/Homework Stuff/homework2")
getwd()
install.packages('foreign')
library('foreign')
sccs <- read.dta(file="SCCS2_v12.dta")
attach(sccs)
sccs

# rename age and tc variables
age = sccs$age
tc = sccs$tc
# order by the x variable otherwise plots will get messed up
tc = tc[order(age)]
age = age[order(age)]

# first thing to do --> plot outcome against covariate to see if there is any relationship
plot(age,tc, ylab="Total Cholesterol", xlab="Age", main="Total Cholesterol vs. Age")

#fit linear regression --> linear age
linfit = lm(tc~age)
summary(linfit)
# fit linear & quadratic 
age2 = age^2
quadfit = lm(tc~age + age2)
summary(quadfit)
# linear spline is run by manually creating new variables
age40 <- (age - 40)
age40[age40 < 0] <- 0
age60 <- (age-60)
age60[age60 < 0] <- 0
#fit the linear spline:
linearsplinefit <- lm( tc ~ age + age40 + age60 )
summary(linearsplinefit)
# fit cubic spline  
age3 <- age^3
age40cube <- age40^3
age60cube <- age60^3
cubesplinefit <- lm( tc ~ age + age2 + age3 + age40cube + age60cube )
summary( cubesplinefit ) 

#plot all models on one scatterplot
plot(age,tc,xlab="Age (years)",ylab="Total Cholesterol", main="Total Cholesterol vs. Age") #scatter plot of outcome and covariate
lines(age,predict(linfit),col="blue") #plots linear fit
lines(age,predict(cubesplinefit),col="green") #plots cubic spline
lines(age,predict(quadfit),col="red") #plots fit of quadratic distribution
#add legend
legend(60,10, c("Linear Regression", "Cubic Spline", "Quadratic Regression"), lty=c(1,1,1), lwd=c(1,1,1),col=c("blue","green", "red"))

# linear and quadratic age plus 

anova(quadfit, cubesplinefit) 

# Question 2: BMI vs TC 
sccs$height_m <- (sccs$height)/100 # height is in cm so change to meters
sccs$bmi <- (sccs$weight)/((sccs$height_m)^2)

lm(tc ~ sccs$bmi)
summary(lm(tc ~ sccs$bmi))

gender <- split(sccs, gender)
females <- gender[[1]]
females_ordered <- females[order(females$age),]
males <- gender[[2]]
males_ordered <- males[order(males$age),]
# scatter plots, tc vs bmi 
# for men 
plot(sccs$bmi[sccs$gender=='Male'], sccs$tc[sccs$gender=='Male'], main="Total Cholesterol vs. Body Mass Index ", ylab="Total Cholesterol", xlab="BMI")
# for women 
points(sccs$bmi[sccs$gender=='Female'],sccs$tc[sccs$gender=='Female'] , col='Red')
legend(33,10, c("Males", "Females"), lty=c(1,1), lwd=c(1,1),col=c("Black","Red"))

summary(lm(tc ~ sccs$bmi))
bmi2 <- (sccs$bmi)^2
summary(lm(tc ~ sccs$bmi + bmi2))
summary(lm(tc ~ sccs$bmi + bmi2 + age))
summary(lm(tc ~ sccs$bmi + bmi2 + age + sccs$gender))

summary(lm(tc ~ sccs$bmi + bmi2 + sccs$gender + sccs$age))


final_fit <- lm(tc ~ sccs$bmi + bmi2 + sccs$gender + sccs$age + (sccs$age)^2 + (sccs$gender)*(sccs$age) + (sccs$gender)*((sccs$age)^2))

bmi = sccs$bmi
bmi22 <- (bmi - 22)
bmi22[bmi22 < 0] <- 0
bmi31 <- (bmi-60)
bmi31[bmi31 < 0] <- 0
cubesplinefit <- lm( tc ~ age + age2 + age3 + age40cube + age60cube )


predict(final_fit, sccs, interval="confidence")
