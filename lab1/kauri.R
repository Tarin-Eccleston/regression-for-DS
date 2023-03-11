# Admin
setwd("/Users/tarineccleston/Documents/Masters/STATS 762/Exercises/Kauri")

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





