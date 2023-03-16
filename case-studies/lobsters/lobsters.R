lobster.df <- read.csv("lobster.csv", header = TRUE)

# Calculate survival percentage
lobster.df$survival <- (lobster.df$survived/lobster.df$n) * 100

lobster.fit <- lm(survival ~ size, data = lobster.df)

# Analysis
plot(survival ~ size, data = lobster.df, xlab = "Body size group (mm)", ylab = "Percentage survival (%)")
abline(coef(lobster.fit)[1], coef(lobster.fit)[2])
summary(lobster.fit)

__________

# Try binomial, better fit to the experiment
# lobster.df$died <- (-lobster.df$survived+lobster.df$n)
# lobster.fit2 <- glm(survived ~ died, data = lobster.df, family="binomial")
