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
cbind(chd.df$age, fitted(chd_fit))

# flipping "coin" for each person from age 20 -> 63
n_people = nrow(chd_df)
new_chd = rbinom(n_people, 1, predict(chd_fit, type = "response"))

# comparison between real and simulated responce
par(mfrow = c(1, 2))
plot(chd ~ age, data = chd_df, main = "Original")
yy = predict(chd_fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)
plot(new_chd ~ age, data = chd.df, main = "Simulated")
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

summary(fit_boot)
# Calculate standard dev from bootstrapping
# Result is similar to that from the summary of the lm
apply(coef_boot, 2, sd)
