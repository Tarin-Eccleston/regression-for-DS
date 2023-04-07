setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/frog-noises")

# Q1a)

frog_df = read.csv("data/frogs.csv")

# find the absolute distance between the frog and each microphone
frog_df$dist = sqrt((abs(frog_df$mic.x - frog_df$animal.x)^2) + (abs(frog_df$mic.y - frog_df$animal.y)^2))
# get the probability of that particular microphone detected that particular frog
# based on the number of calls the frog makes and the detected amount from the mic
frog_df$prob = frog_df$detected / frog_df$n.calls

plot(prob ~ dist, main = "Distance of Frog from Mic vs Probability of Noise Detection", xlab = "Distance of Frog from Mic (m)", ylab = "Probability of Noise Detection", data = frog_df)

# fit model
frog_fit <- glm(cbind(detected, n.calls - detected) ~ dist, family = "binomial", data = frog_df)

summary(frog_fit)
xx <- seq(min(frog_df$dist), max(frog_df$dist), length.out = 1000)
yy <- predict(frog_fit, newdata = data.frame(dist = xx), type = "response")
lines(xx, yy)               

# Q1b)



