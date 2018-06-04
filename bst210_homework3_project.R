getwd()
setwd("/Users/isabelle/Desktop/Harvard/Fall2016/BST210/")
fisherman <- read.csv("fisherman_data.csv")
head(fisherman)
fisherman$bmi=fisherman$weight/(fisherman$height/100)^2

# split into fisherman and control



# look at some plots to see potential association

# Residence Time vs. Total Mercury 
plot(fisherman$restime, fisherman$TotHg, xlab = 'Residence Time (years) ', ylab = 'Total Mercury (mg/g)', main = 'Total Mercury vs. Residence Time' , col = 'blue', pch = 8)
# Split Control & Fishermen 
res_time_fish = fisherman$restime[fisherman$fisherman == 1]
res_time_cont = fisherman$restime[fisherman$fisherman == 0]
tot_merc_fish = fisherman$TotHg[fisherman$fisherman == 1]
tot_merc_cont = fisherman$TotHg[fisherman$fisherman == 0]
plot(res_time_fish, tot_merc_fish, xlab = 'Residence Time (years) ', ylab = 'Total Mercury (mg/g)', main = 'Total Mercury vs. Residence Time in Fishermen' , col = 'blue', pch = 8)
plot(res_time_cont, tot_merc_cont, xlab = 'Residence Time (years) ', ylab = 'Total Mercury (mg/g)', main = 'Total Mercury vs. Residence Time in Controls' , col = 'blue', pch = 8)

# All 
allfit = lm(fisherman$MeHg~fisherman$height+fisherman$weight+fisherman$fishmlwk+fisherman$age+fisherman$restime+fisherman$fishpart)
quadfit = lm(fisherman$MeHg~fisherman$height+fisherman$weight+fisherman$fishmlwk)
summary(quadfit)
summary(allfit)

## this gives table with significance for each predictor --> keep 
covariates = c("age", "restime", "bmi", "fishmlwk", "fishpart", "fisherman")
single_res = matrix(NA, nrow = length(covariates), ncol = 4)
rownames(single_res) = covariates
for (i in seq(length(covariates))){
  print(covariates)
  model = lm(formula = paste("MeHg ~", covariates[i]), data = fisherman)
  print(summary(model)$coef[2,])
  single_res[i,] = summary(model)$coef[2,]
}
colnames(single_res) = names(summary(model)$coef[2,])
View(round(single_res, 4))
## 

## this gives table with significance for each predictor --> keep height and weight
covariates2 = c("age", "restime", "height", "weight", "fishmlwk", "fishpart", "fisherman")
single_res2 = matrix(NA, nrow = length(covariates2), ncol = 4)
rownames(single_res2) = covariates2
for (i in seq(length(covariates2))){
  print(covariates2)
  model2 = lm(formula = paste("MeHg ~", covariates2[i]), data = fisherman)
  print(summary(model2)$coef[2,])
  single_res2[i,] = summary(model2)$coef[2,]
}
colnames(single_res2) = names(summary(model2)$coef[2,])
View(round(single_res2, 4))

## Model Building - Add/Drop ##
library(MASS)
# Fit a full model
fit.full = lm(paste("MeHg ~ ", paste(covariates, collapse = '+')), data = fisherman)
summary(fit.full)

# Drop and add one covariate at a time based on AIC
drop1(fit.full)
add1(lm(MeHg ~ 1, data = fisherman), formula(paste("~", paste(covariates, collapse = "+"))))
## 

# Including quadratic terms for continuous terms 
quadfit1 = lm(fisherman$MeHg~fisherman$bmi+(fisherman$bmi)^2+fisherman$fishmlwk)
summary(quadfit1)
c("age", "restime", "bmi", "fishmlwk", "fishpart", "fisherman")

library(leaps)
best.adjr2 = regsubsets(as.formula(paste("MeHg ~ ", paste(covariates, collapse = '+'))), 
                        data = fisherman, nbest = 1)
summary.out = summary(best.adjr2)
summary.out$which[which.max(summary.out$adjr2),]
best_mod_adjr2 = lm(totchol ~ sex + age + sysbp + diabp + cigpday + bmi + diabetes + prevchd, data = framingham)
summary(best_mod_adjr2)




## Part 2 
fishermen <- subset(fisherman, fisherman == 1)
controls <- subset(fisherman, fisherman == 0)

# Table of Mean values for all covariates 

mean(fishermen$MeHg)
mean(controls$MeHg)
mean(fishermen$fishmlwk)
mean(controls$fishmlwk)

## For part 2, create models for each dataset --> fishermen and controls and compare which covariates are significant in each

## FISHERMEN ## 
null <- lm(formula = fishermen$MeHg~1, data = fishermen)

# set quadratic terms to be potentially included in model
# (only include continuous variables)
age2 <- (fishermen$age)^2
rest2 <- (fishermen$restime)^2
height2 <- (fishermen$height)^2
weight2 <- (fishermen$weight)^2
mlwk2 <- (fishermen$fishmlwk)^2

# full model
full <- lm(formula = fishermen$MeHg~ fishermen$age + age2 + fishermen$restime + rest2 + fishermen$height + height2 + fishermen$weight + weight2 + fishermen$fishmlwk + mlwk2 + fishermen$fishpart)
summary(full) # nothing is significant 

# forward stepwise selection
forward_fish <- step(null, scope=list(lower=null, upper=full), direction="forward")
# backward stepwise selection
backward_fish <- step(full, direction="backward")

## CONTROLS ## 
null2 <- lm(formula = controls$MeHg~1, data = controls)

# set quadratic terms to be potentially included in model
# (only include continuous variables)
age22 <- (controls$age)^2
rest22 <- (controls$restime)^2
height22 <- (controls$height)^2
weight22 <- (controls$weight)^2
mlwk22 <- (controls$fishmlwk)^2

# full model
full2 <- lm(formula = controls$MeHg~ controls$age + age22 + controls$restime + rest22 + controls$height + height22 + controls$weight + weight22 + controls$fishmlwk + mlwk22 + controls$fishpart)
summary(full2) # nothing is significant 

# forward stepwise selection
forward_control <- step(null2, scope=list(lower=null2, upper=full2), direction="forward")
# backward stepwise selection
backward_control <- step(full2, direction="backward")

