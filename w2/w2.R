setwd("/Users/tarineccleston/Documents/Masters/STATS 762/Exercises/W2")

# A bit of a throwback
chd.df <- read.table("chd.data", header = TRUE)
chd.fit <- glm(chd ~ age, family = "binomial", data = chd.df)

summary(chd.fit)
plot(chd ~ age, data = chd.df)
xx <- seq(10, 80, length.out = 1000)
yy <- predict(chd.fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)
  
# How do we estimate the age which you are 80% likely to get CHD?
# Bootstrapping is simulating data consistent with the model instead of collecting more data.
# Assuming model is true

# Create new matrix, age from sample, and predicted likelihood of chd to the response
cbind(chd.df$age, fitted(chd.fit))

# Flipping "coin" for each person from age 20 -> 63
n.people <- nrow(chd.df)
new.chd <- rbinom(n.people, 1, predict(chd.fit, type = "response"))

# Comparison between real and simulated responce
par(mfrow = c(1, 2))
plot(chd ~ age, data = chd.df, main = "Original")
yy <- predict(chd.fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)
plot(new.chd ~ age, data = chd.df, main = "Simulated")
new.fit <- glm(new.chd ~ age, family = binomial("probit"), data = chd.df)
yy <- predict(new.fit, newdata = data.frame(age = xx), type = "response")
lines(xx, yy)

# Create multiple simulations to calculate estimates for int and co-eff, and then calculate SE, SD
# Can run simulations instead of calculating values using maths/theory
n.boots <- 10000
coef.boot <- matrix(0, nrow = n.boots, ncol = 2)
agep80.boot <- numeric(n.boots)
for (i in 1:n.boots){
  chd.boot <- rbinom(n.people, 1, predict(chd.fit, type = "response"))
  fit.boot <- glm(chd.boot ~ age, family = binomial(link = "probit"), data = chd.df)
  coef.boot[i, ] <- coef(fit.boot)
  agep80.boot[i] <- (0.84162 - coef(fit.boot)[1])/coef(fit.boot)[2]
}

summary(fit.boot)
# Calculate standard dev from bootstrapping
# Result is similar to that from the summary of the lm
apply(coef.boot, 2, sd)

