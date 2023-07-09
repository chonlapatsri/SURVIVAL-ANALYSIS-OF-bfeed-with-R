## clear list
rm(list = ls())
## Install survival and KMsurv library
library(survival)
library(KMsurv)

## Define bfeed as a data frame, attach it the the script so we do not have to rewrite it everytime we use the data frame
data(bfeed)
attach(bfeed)

## Call bfeed to look at the data
bfeed

## See the number of row, the number of censored data, and the summary of bfeed
nrow(bfeed)
nrow(subset(bfeed, delta=="0"))
summary(bfeed)
## There are 927 rows = 927 samples
## It can be found that race, poverty, smoke, alcohol, and pc3mth which are factors are still treated as numerical features

## Convert those features to factor
bfeed$race <- factor(bfeed$race)
bfeed$poverty <- factor(bfeed$poverty)
bfeed$smoke <- factor(bfeed$smoke)
bfeed$alcohol <- factor(bfeed$alcohol)
bfeed$pc3mth <- factor(bfeed$pc3mth)


## Check the summary again, now they all are factors
summary(bfeed)
bfeed

## We study the distribution of the age of mother at birth of child
hist(agemth, freq = FALSE, xlab = "Age (years)")
plot(ybirth, duration)
## From the histogram plot, It is found that the sample group's age forms a normal distribution from the min of 15 to max of 28 years old
## From the dot plot, we can already identify some outliers as there are just three 18 yrs old and five 28 yrs old 
## and seven samples who breast feed near and more than 100 weeks out of total 927.

## We study the distribution of the education level of mother too
hist(yschool, freq = FALSE, xlab = "yschool")
plot(yschool, duration)

## Now, let's see if there are any noticeable information on duration of breastfeed on each factor using box plots
boxplot(duration ~ race)
boxplot(duration ~ poverty)
boxplot(duration ~ smoke)
boxplot(duration ~ alcohol)
boxplot(duration ~ pc3mth)
## there is not a significant information we can acquire from this exercise except 
## that the distribution of the "other" race is narrower than the other two which is not very useful

### Survival analysis ###

## Start with the Kaplan-Meier analysis on the general model
fit <- survfit(Surv(duration, delta) ~ 1, data = bfeed, conf.type = "log")
summary(fit)
## Since the upper 95% CI is never above 1, it is safe to use the log method 

## Followed by the Kaplan-Meier curve 
plot(fit, conf.int = 0.95, conf.type= "log", ylim = (0.0:1.0), mark.time = FALSE, col = 1:1, xlab = "Duration", ylab = "Survival")

## Now try with log-log to see the difference
fit_log2 <- survfit(Surv(duration, delta) ~ 1, data = bfeed, conf.type = "log-log")
summary(fit_log2)

plot(fit_log2, conf.int = 0.95, conf.type= "log-log", ylim = (0.0:1.0), mark.time = FALSE, col = 1:1, xlab = "Duration", ylab = "Survival")

## Next we will look at the effect of each feature to the Survivability with log-rank test

## race
fit_race <- survfit(Surv(duration, delta) ~ race, data = bfeed, conf.type = "log")
summary(fit_race)
plot(fit_race, conf.int = 0.95, conf.type= "log", ylim = (0.0:1.0),mark.time = FALSE, col = 1:3, xlab = "Duration", ylab = "Survival", main = "KPM by race")
legend("topright", legend = unique(bfeed$race), lty = 3, col = 1:3, bty = "n", ncol =3)
diff_race <- survdiff(Surv(duration, delta) ~race, data = bfeed)
diff_race
## from the result, we see that the Chi^2 is above 3.841 and the p-value < 0.05 
## The indication that we can reject the null hypothesis
## We can also see that race 1 (white)'s observed breastfeeding completion is less than the expected number, whereas it is higher for the other two races 

## poverty
fit_poor <- survfit(Surv(duration, delta) ~ poverty, data = bfeed, conf.type = "log")
summary(fit_poor)
plot(fit_poor, conf.int = 0.95, conf.type= "log", ylim = (0.0:1.0),mark.time = FALSE, col = 1:2, xlab = "Duration", ylab = "Survival", main = "KPM by poverty")
legend("topright", legend = unique(bfeed$poverty), lty = 2, col = 1:2, bty = "n", ncol =2)
diff_poor <- survdiff(Surv(duration, delta) ~poverty, data = bfeed)
diff_poor
## from the result, we see that the Chi^2 is below 3.841 and the p-value > 0.05 
## The indication that we cannot reject the null hypothesis

## smoke
fit_smoke <- survfit(Surv(duration, delta) ~ smoke, data = bfeed, conf.type = "log")
summary(fit_smoke)
plot(fit_smoke, conf.int = 0.95, conf.type= "log", ylim = (0.0:1.0),mark.time = FALSE, col = 1:2, xlab = "Duration", ylab = "Survival", main = "KPM by smoke")
legend("topright", legend = unique(bfeed$smoke), lty = 2, col = 1:2, bty = "n", ncol =2)
diff_smoke <- survdiff(Surv(duration, delta) ~smoke, data = bfeed)
diff_smoke
## from the result, we see that the Chi^2 is above 3.841 and the p-value < 0.05 
## The indication that we can reject the null hypothesis
## We can also see that the non-smoking group has more samples with complete breastfeeding than expected, while the smoking group has less

## alcohol
fit_alcohol <- survfit(Surv(duration, delta) ~ alcohol, data = bfeed, conf.type = "log")
summary(fit_alcohol)
plot(fit_alcohol, conf.int = 0.95, conf.type= "log", ylim = (0.0:1.0),mark.time = FALSE, col = 1:2, xlab = "Duration", ylab = "Survival", main = "KPM by alcohol")
legend("topright", legend = unique(bfeed$alcohol), lty = 2, col = 1:2, bty = "n", ncol =2)
diff_alcohol <- survdiff(Surv(duration, delta) ~alcohol, data = bfeed)
diff_alcohol
## from the result, we see that the Chi^2 is below 3.841 and the p-value > 0.05 
## The indication that we cannot reject the null hypothesis 

## pc3mth
fit_pc3mth <- survfit(Surv(duration, delta) ~ pc3mth, data = bfeed, conf.type = "log")
summary(fit_pc3mth)
plot(fit_pc3mth, conf.int = 0.95, conf.type= "log", ylim = (0.0:1.0),mark.time = FALSE, col = 1:2, xlab = "Duration", ylab = "Survival", main = "KPM by parental care after 3rd month")
legend("topright", legend = unique(bfeed$pc3mth), lty = 2, col = 1:2, bty = "n", ncol =2)
diff_pc3mth <- survdiff(Surv(duration, delta) ~pc3mth, data = bfeed)
diff_pc3mth
## from the result, we see that the Chi^2 is below 3.841 and the p-value > 0.05 
## The indication that we cannot reject the null hypothesis

## It seems that we can disregard poverty, alcohol, and the parental care influence in the completion of breastfeeding
## However, let's see if the Cox proportional hazards regression model agree with this conclusion

cox_fit_gen <- coxph(Surv(duration, delta) ~ race + poverty + smoke + alcohol + agemth + ybirth + yschool + pc3mth, data = bfeed)
cox_fit_gen
## we have an agreement on alcohol and parental care but not on poverty as its Wald test p-value is < 0.05, so let's also use the likelihood ratio test to be more confident
cox_fit_no_alcohol <- update(cox_fit_gen,. ~ . - alcohol)
anova(cox_fit_gen,cox_fit_no_alcohol)
cox_fit_no_pc3mth <- update(cox_fit_gen,. ~ . - pc3mth)
anova(cox_fit_gen,cox_fit_no_pc3mth)
# From the result, we can conclude that alcohol and pc3mth can be dropped as their likelihood ratio test's p-values are close to their Wald test's 

## Update the gen model to remove alcohol and pc3mth and do the Wald test again to see the p-value of poverty
cox_fit_gen <- coxph(Surv(duration, delta) ~ race + poverty + smoke + agemth + ybirth + yschool, data = bfeed)
cox_fit_gen
## It has increased but still < 0.05 which, and we still cannot disregard it

## Let's also do the likelihood ratio test to confirm
cox_fit_no_poverty <- update(cox_fit_gen,. ~ . - poverty)
anova(cox_fit_gen,cox_fit_no_poverty)
## It is said that poverty feature is statistically significant despite the suggestion of the log-rank test to be otherwise

## From the Wald test, it suggests that the feature "age of mother at birth of child" might not be significant
## Let's apply the likelihood ratio test to confirm
cox_fit_no_agemth <- update(cox_fit_gen,. ~ . - agemth)
anova(cox_fit_gen,cox_fit_no_agemth)
## We can conclude that the age feature is not statistically significant and can be disregarded

## now we update the general Cox proportional hazards regression model to disregard the agemth
cox_fit_gen <- update(cox_fit_gen,. ~ . - agemth)
cox_fit_gen

## From the Wald test, it is also said that p-value of race 2 is not statistically significant, however it is not possible to drop as race3 is significant
## Therefore, we will try combining race2 and race3 such that the race feature contains only 1=white and 2=other
race2 <- race
race2
race2[race2 == 3] <- 2
race2

## Create a general Cox proportional hazards regression model of the data frame with new race column
cox_fit_new_race <- update(cox_fit_gen,. ~ . - race + factor(race2))
cox_fit_new_race
## The Wald test's p-value of the new race column is < 0.05, which says that it is significant

## Check the two models with the Akaike Information Criterion as the new model is not a nested model of the general one 
AIC(cox_fit_gen,cox_fit_new_race)
## We found that the drop in AIC is 1 which means the model with race feature is slightly better

## Let's test again with the general and nested models
cox_fit_no_race <- update(cox_fit_gen,. ~ . - race)
anova(cox_fit_gen,cox_fit_no_race)
## The Likelihood ratio test suggested still that feature "race" is significant, therefore we cannot reject the null hypothesis as all test led to the same conclusion

## Therefore, we have our best fit model below (with new race column)
cox_fit_best <- coxph(Surv(duration, delta) ~ race + poverty + smoke + ybirth + yschool - race + factor(race2), data = bfeed)
cox_fit_best

## Also get the general model back just in case we need it
cox_fit <- coxph(Surv(duration, delta) ~ race + poverty + smoke + alcohol + agemth + ybirth + yschool + pc3mth, data = bfeed)
cox_fit

## Next we use the residual to check the quality of the model

## 1st, we try martingale residual
resDev <- residuals(cox_fit_best, "martingale") 
plot(resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(resDev), col = "orange", lwd = 2)

## plot against each numerical feature
#duration
plot(duration, resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(duration, resDev), col = "orange", lwd = 2)
## we found that the model does not conform to linearity when it is duration feature

#poverty
plot(poverty, resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(poverty, resDev), col = "orange", lwd = 2)

#smoke
plot(smoke, resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(smoke, resDev), col = "orange", lwd = 2)

#agemth
plot(agemth, resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(agemth, resDev), col = "orange", lwd = 2)

#ybirth
plot(ybirth, resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(ybirth, resDev), col = "orange", lwd = 2)

#yschool
plot(yschool, resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(yschool, resDev), col = "orange", lwd = 2)

#race2
plot(race2, resDev, main = "Martingale residual", pch = 15, col = 1 + delta)
lines(lowess(race2, resDev), col = "orange", lwd = 2)


## 2nd we use deviance residual
resDev <- residuals(cox_fit_best, "deviance") 
plot(resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(resDev), col = "blue", lwd = 2)

## plot against each numerical feature
#duration
plot(duration, resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(duration, resDev), col = "blue", lwd = 2)
## Onceagain, we found that the model does not conform to linearity when it is duration feature

#poverty
plot(poverty, resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(poverty, resDev), col = "blue", lwd = 2)

#smoke
plot(smoke, resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(smoke, resDev), col = "blue", lwd = 2)

#agemth
plot(agemth, resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(agemth, resDev), col = "blue", lwd = 2)

#ybirth
plot(ybirth, resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(ybirth, resDev), col = "blue", lwd = 2)

#yschool
plot(yschool, resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(yschool, resDev), col = "blue", lwd = 2)

#race2
plot(race2, resDev, main = "Deviance residual", pch = 15, col = 1 + delta)
lines(lowess(race2, resDev), col = "blue", lwd = 2)


## Finally we check the proportional hazard assumption 
plot(cox.zph(cox_fit_best))

#poverty
cox_fit_poverty <- coxph(Surv(duration, delta) ~ poverty, data = bfeed)
plot(cox.zph(cox_fit_poverty))

#smoke
cox_fit_smoke <- coxph(Surv(duration, delta) ~ smoke, data = bfeed)
plot(cox.zph(cox_fit_smoke))

#ybirth
cox_fit_ybirth <- coxph(Surv(duration, delta) ~ ybirth, data = bfeed)
plot(cox.zph(cox_fit_ybirth))

#yschool
cox_fit_yschool <- coxph(Surv(duration, delta) ~ yschool, data = bfeed)
plot(cox.zph(cox_fit_yschool))

#factor(race2)
cox_fit_new_race <- coxph(Surv(duration, delta) ~ factor(race2), data = bfeed)
plot(cox.zph(cox_fit_new_race))

## We can conclude that the assumption is satisfied

