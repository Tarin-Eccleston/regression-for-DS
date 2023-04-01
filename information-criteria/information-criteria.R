setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/information-criteria")

X <- read.csv("data/X.csv", header = TRUE)

# 1) 
pairs(X)

# 2) 

beta_true = c(10, 2, 3, 1)
sigma = 4

# generate regression line based on the given "true" parameters
mu = beta_true[1] + beta_true[2]*X$X1 + beta_true[3]*X$X2 + beta_true[4]*X$X1*X$X2

# simulate of the normal distribution
X$Y = rnorm(nrow(X), mu, 4)

pairs(X)

# 3)
model = lm(Y ~ X1*X2, data = X)
beta_hat = summary(model)$coef[,1]

confint(model,"X1", level=0.95)

