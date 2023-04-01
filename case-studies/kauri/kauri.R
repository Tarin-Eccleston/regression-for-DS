# Admin
setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/case-studies/kauri")

# Q1
kauri.df <- read.csv("kauri.csv", header = TRUE)
plot(presence ~ altitude, data = kauri.df, xlab = "Altitude (m)", ylab = "Presence (Yes/No)")

# Q2
kauri.fit <- glm(presence ~ altitude, data = kauri.df, family = "binomial")
summary(kauri.fit)

# Fit regression curve
xx <- seq(0, 600, length.out = 1000)
yy <- predict(kauri.fit, newdata = data.frame(altitude = xx), type = "response")
lines(xx, yy)

# Q3
# Make new dataset
n.kauri <- nrow(kauri.df)
new.kauri <- rbinom(n.kauri, 1, predict(kauri.fit, type = "response"))

# Compare real data wiuth simulated data
par(mfrow = c(2, 2))
plot(presence ~ altitude, data = kauri.df, main = "Original")
yy <- predict(kauri.fit, newdata = data.frame(presence = xx), type = "response")
lines(xx, yy)

plot(new.kauri ~ altitude, data = kauri.df, main = "Simulated 1")
# new.fit <- glm(new.kauri ~ altitude, family = binomial("probit"), data = kauri.df)
new.fit <- glm(new.kauri ~ altitude, family = "binomial", data = kauri.df)
yy <- predict(new.fit, newdata = data.frame(altitude = xx), type = "response")
lines(xx, yy)

n.kauri <- nrow(kauri.df)
new.kauri <- rbinom(n.kauri, 1, predict(kauri.fit, type = "response"))

plot(new.kauri ~ altitude, data = kauri.df, main = "Simulated 2")
# new.fit <- glm(new.kauri ~ altitude, family = binomial("probit"), data = kauri.df)
new.fit <- glm(new.kauri ~ altitude, family = "binomial", data = kauri.df)
yy <- predict(new.fit, newdata = data.frame(altitude = xx), type = "response")
lines(xx, yy)

n.kauri <- nrow(kauri.df)
new.kauri <- rbinom(n.kauri, 1, predict(kauri.fit, type = "response"))

plot(new.kauri ~ altitude, data = kauri.df, main = "Simulated 3")
# new.fit <- glm(new.kauri ~ altitude, family = binomial("probit"), data = kauri.df)
new.fit <- glm(new.kauri ~ altitude, family = "binomial", data = kauri.df)
yy <- predict(new.fit, newdata = data.frame(altitude = xx), type = "response")
lines(xx, yy)

# Simulated data has a stronger relationshop between altitude and presence. However this could be by chance
# Try bootstrapping to confirm / disprove this
# Greater variance at higher altitude -> poisson?????

# Bootstrapping
n.boots <- 10000
coef.boot <- matrix(0, nrow = n.boots, ncol = 2)
agep80.boot <- numeric(n.boots)
for (i in 1:n.boots){
  kauri.boot <- rbinom(n.kauri, 1, predict(kauri.fit, type = "response"))
  fit.boot <- glm(kauri.boot ~ altitude, family = "binomial", data = kauri.df)
  coef.boot[i, ] <- coef(fit.boot)
  agep80.boot[i] <- (0.84162 - coef(fit.boot)[1])/coef(fit.boot)[2]
}

# Lab 3

# Q1
# we use the square root, as we DON'T want a symmetric response, as there are some 
# kauri at higher altitudes as well
kauri_df <- read.csv("data/kauri.csv", header = TRUE)
kauri_fit <- glm(presence ~ sqrt(altitude) + altitude, data = kauri_df, family = "binomial")

par(mfrow = c(1, 1))
plot(presence ~ altitude, data = kauri_df, xlab = "Altitude (m)", ylab = "Presence (Yes/No)")
xx <- seq(0, 600, length.out = 1000)
yy <- predict(kauri_fit, newdata = data.frame(altitude = xx), type = "response")
lines(xx, yy)

# Q2

alpha_hat = kauri_fit$coefficients[2]^2 / (4 * kauri_fit$coefficients[3]^2)
# The optimum altitude is 164 m
abline(v = alpha_hat)

# Q3
# compute SE and confidence interval

# use parametric bootstrapping
n_kauri = nrow(kauri_df)
n_boots = 10000
coef_boot = matrix(0, nrow = n_boots, ncol = 3)
# make vector of altitudes for prediction
altitude_best_boot = numeric(n_boots)

for (i in 1:n_boots){
  kauri_boot = rbinom(n_kauri, 1, predict(kauri_fit, type = "response"))
  fit_boot = glm(presence ~ sqrt(altitude) + altitude, family = binomial(link = "logit"), data = kauri_df)
  coef_boot[i, ] = coef(fit_boot)
  altitude_best_boot[i] = fit_boot$coefficients[2]^2 / (4 * fit_boot$coefficients[3]^2)
}

altitude_best_boot_es = fit_boot$coefficients[2]^2 / (4 * fit_boot$coefficients[3]^2)
# standard error
altitude_best_se = sd(altitude_best_boot)

# we can use standard theory (i.e assuming a normal sampling dist) to get a
# confidence interval
conf_int = c(altitude_best_se - qnorm(0.975)*altitude_best_se, altitude_best_se + 1.96*altitude_best_se)

abline(v = conf_int[1])
abline(v = conf_int[2])


