# Parametric bootstrapping

setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/w2")

# a bit of a throwback
chd_df = read.table("chd.data", header = TRUE)
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