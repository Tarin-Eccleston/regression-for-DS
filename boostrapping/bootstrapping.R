### Bootstrapping

# Bootstrapping is especially useful in cases where
#   - It is difficult or impossible to obtain a sampling dist based on theory
#   - The assumptions required to obtain a theoretical sampling dist are in doubt

# Note: 

# The bootstrap is NOT used to get a better estimate of a statistic - the
# expected value of the boostrap estimate is equal to the sample estimate

# The data must be independent and representive of the population

# Does not compensate for the model misspecification

# ______________________________________________________________________________

## Parametric bootstrapping

setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/bootstrapping")

# a bit of a throwback
chd_df = read.table("data/chd.data", header = TRUE)
chd_fit = glm(chd ~ age, family = "binomial", data = chd_df)

summary(chd_fit)
plot(chd ~ age, data = chd_df)
xx = seq(10, 80, length.out = 1000)
yy = predict(chd_fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)
  
# How do we estimate the age which you are 80% likely to get CHD?
# Bootstrapping is simulating data consistent with the model instead of collecting more data.
# Assuming model is true

# Create new matrix, age from sample, and predicted likelihood of chd to the response
cbind(chd_df$age, fitted(chd_fit))

# flipping "coin" for each person from age 20 -> 63
n_people = nrow(chd_df)
new_chd = rbinom(n_people, 1, predict(chd_fit, type = "response"))

# comparison between real and simulated responce
par(mfrow = c(1, 2))
plot(chd ~ age, data = chd_df, main = "Original")
yy = predict(chd_fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)
plot(new_chd ~ age, data = chd_df, main = "Simulated")

# fit binomial model
new_fit = glm(new_chd ~ age, family = binomial("probit"), data = chd_df)
yy = predict(new_fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)

# create multiple simulations to calculate estimates for int and co-eff, and then calculate SE, SD
# can run simulations instead of calculating values using maths/theory
n_boots = 10000
coef_boot = matrix(0, nrow = n_boots, ncol = 2)
# vector to store estimates of the age at which the chance of having CHD is 80%
agep80_boot = numeric(n_boots)
for (i in 1:n_boots){
  chd_boot = rbinom(n_people, 1, predict(chd_fit, type = "response"))
  fit_boot = glm(chd_boot ~ age, family = binomial(link = "probit"), data = chd_df)
  coef_boot[i, ] = coef(fit_boot)
  agep80_boot[i] = (0.84162 - coef(fit_boot)[1])/coef(fit_boot)[2]
}
  
# Let's see how we did with the estimated coefficients, comparing them to theory

# SE from summary()
summary(chd_fit)$coef[,2]
# SE from the boostrap
apply(coef_boot, 2, sd)
## not sure why the standard errors are really far apart

# estimate from the real data
agep80_est = (0.84162 - coef(chd_fit)[1]/coef(chd_fit)[2])
agep80_est
# the standard error calculated by bootstrap
agep80_se = sd(agep80_boot)

# we can use standard theory (i.e assuming a normal sampling dist) to get a
# confidence interval
c(agep80_est - qnorm(0.975)*agep80_se, agep80_est + 1.96*agep80_se)

hist(agep80_boot, breaks = 20)

# there's a better way to do all of this which doesn't require us to assume the sampling 
# distribution for the estimator (beta coefficients) is normal
# the histogram shows that the data is slightly right-skewed

# easier way to get confidence intervals
# confidence interval for Beta0
quantile(coef_boot[, 1], probs = c(0.025, 0.975))
# confidence interval for Beta1
quantile(coef_boot[, 1], probs = c(0.025, 0.975))
# confidence interval for the age at which CHD prob is 80% is...
quantile(coef_boot[, 1], probs = c(0.025, 0.975))

# normal Q-Q plots can be used to visualize data skew. if the slope is straight
# then the data is not skewed

# ______________________________________________________________________________

## Non-parametric Bootstrapping

# the difference between parametric and non-parametric is we resample from
# the original data set
# mimics sampling from the population

# sample random numbers from 0 - 100, corresponding to the data rows in the 
# original data set

# for one iteration...
# s = sample(n_people, replace = TRUE)
# chd_df_np_boot = chd_df[s,]

n_np_boots = 10000

coef_np_boots = matrix(0, nrow = n_boots, ncol = 2)
agep80_np_boot = numeric(n_boots)

# for 10000 iterations
for (i in 1:n_np_boots) {
  # only difference between non-parametric and parametric is this line of code
  chd_df_np_boot = chd_df[sample(n_people, replace = TRUE),]

  fit_np_boot = glm(chd ~ age, family = binomial(link = "probit"), data = chd_df_np_boot)
  coef_np_boots[i,] = coef(fit_np_boot)
  agep80_np_boot = (0.84162 - coef(fit_np_boot)[1]/coef(fit_np_boot)[2])
}

# compare standard deviations
# the standard deviations between parametric and non-parametric bootstrapping
# are quite similar. The SD is a measure of how much variance there is for beta
# values in the simulated data
apply(coef_np_boots, 2, sd)
apply(coef_boot, 2, sd)

# Notes: non-parametric bootstrapping is gold standard as you are not relying 
# on the accuracy of your model to simulate new data
