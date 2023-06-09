# Introduction to Linear, Poisson and Binomial models

setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/linear-models")

## Catheter Length
# Dealing with continuous data (can only be positive)
# Constant variance

catheter.df <- read.table("data/catheter.data", header = TRUE)

# Fit a linear model.
catheter.fit <- lm(ca ~ ht, data = catheter.df)

# Std. Error`: The `std` of coefficients if we repeat the sampling from the population infinite times.
summary(catheter.fit)

# MUST Label Axis
plot(ca ~ ht, data = catheter.df, xlab = "Height (inches)", ylab = "Catheter Lenght (inches)")
abline(coef(catheter.fit)[1], coef(catheter.fit)[2])

# ____________________________________________________
## Crabs
# "sats": number of male crabs concentrating around the female crab (number of satellite male crabs).
# Dealing with count data (integer values and positive)
# Variance and expected satellite males increases as female body size increase
# Variance = Mean (response)
# Therefore use Poisson distribution
# Variance = mean for Poisson distribution. High expectation = high variance and vise verso

crabs.df <- read.table("crab.data", header = TRUE)
plot(sats ~ weight, data = crabs.df)

# Use linear regression. Poisson regression is most suitable in this case.
# The expected value must be above 0, so linear regression doesn't give meaningful prediction sometimes.
crabs.fit <- lm(sats ~ weight, data = crabs.df)
plot(sats ~ weight, data = crabs.df)
abline(crabs.fit$coefficients[1], crabs.fit$coefficients[2])

# Poisson regression
crabs.logfit <- glm(sats ~ weight, data = crabs.df, family = "poisson")

# We cannot plot curve in R, so use sequence.
xx <- seq(min(crabs.df$weight), max(crabs.df$weight), length.out = 101)
# The linear predictor gives log of mew. Log being the link function. To get mew, don't need to exponentiate, specify type as response.
yy <- predict(crabs.logfit, newdata = data.frame(weight = xx), type = 'response')
plot(sats ~ weight, data = crabs.df)
lines(yy ~ xx)

# ____________________________________________________
## Corenory heart disease, 
# Binomial Distribution: Y = Binomial(n, p)
# N = number of trials / different types of results
# Bernoulli type Binomial: Because there is truly only 0 or 1

chd.df <- read.table("chd.data", header = TRUE)
chd.fit <- glm(chd ~ age, family = "binomial", data = chd.df)

summary(chd.fit)
plot(chd ~ age, data = chd.df)
xx <- seq(min(chd.df$age), max(chd.df$age), length.out = 1000)
yy <- predict(chd.fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)

# Questions: Out of the 12 chickens, how many would be infront of the coop?
# A: Binomial. There's an upper limit. You can calculate proportion.
# In R for binomial, setup success on first column, and failures on second column
# ____________________________________________________
## Chickens (Extra)
# The lecturer has 12 chickens, and he wants predicts how many of them are in the yard at a given time.
# We should use binomial regression, because there is a upper bound. The crab example doesn't have a upper bound (can be infinity).

n.chickens <- cbind(rbinom(8, 12, 0.4))
# Success on first col, failures on second col
chicken <- cbind(n.chickens, 12 - n.chickens)

# ____________________________________________________

# Maximum Likelihood Estimation vs RSS
# ____________________
# For GLMs, the estimated coefficients for the model are usually based on maximum
# likelyhood (ML) estimation. Not RSS
# this is because in a normal regression, the variance is constant, where the 
# variance is not constant in a poisson regression
# Don't need to get into depth with this topic

# Common Approach to getting ML
#   - Basically take partial diff of the ML equation and set to 0 to solve

# For ordinary regression, taking a partial diff, results in. a set of linear equations
# beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y

# BUT for GLMs, we use IRLS algorithm which converges to a correct solution and quickly
# this is used in the glm() function in R

# Coefficients
# ____________________
# normal regression coefficients assume a normal distribution
# glm coefficients do not completely follow the normal distribution as you need
# an asymptotically large sample from the population to get a normal result

# Assessing Fit for GLM
# ____________________
# for GLM, deviance is the basic measure of how well a model fits the data
# for normal regression, we use RSS
# more scatering around the line = more deviance

# Refer to notes




