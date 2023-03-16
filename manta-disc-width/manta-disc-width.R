# Assignment 1: Manta Disk Length

# Question 1(a): Why is a linear model more appropriate than other types of GLMs we’ve met in this course so far?
# Variables which measure size such as width, length and height are typically normally
# distributed when either any of them are the responce variable and the rest are explanatory variables
# therefore it appropriate to assume that the responce distribution for the disc width as explained by the explanatory variables:
# disc length, cranial width and sex is normally distributed.

# Question 1(b): Create plots to investigate relationships between the variables in this data set. Briefly comment on the plots (2–3 sentences).

# Lets do this later

setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/manta-disc-width")
library(tidyverse)

manta_df = read.csv("data/manta-measurements.csv")

# convert F to 0, and M to 1 for the normal regression model
manta_df$sex = gsub('F', 0, manta_df$sex)
manta_df$sex = gsub('M', 1, manta_df$sex)
manta_df$sex = as.numeric(manta_df$sex)

# fitting data
manta_fit = lm(DW ~ DL + CW + sex, data = manta_df)
summary(manta_fit)

# plotting data
# plot(DW ~ DL + CW + sex, data = manta_df, xlab = "Height (inches)", ylab = "Catheter Lenght (inches)")

# Question 1(c): Create the model’s design matrix. Only print out the first six rows.

# append a column of 1s at the beginning for the intercept. remove the response variable (disc width)
# make sure X and Y are in matrix form for the calculation
X = as.matrix(cbind(1, manta_df[,-2]))

# Question 1(d): Calculate βb, the vector of estimated coefficients.
# Calculate βb by using: betahat = (Xt*X)^-1 *Xt*Y
# Where X is the design matrix, and Y is the response variable matrix

Y = as.matrix(manta_df[,2])
# calculation
beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y

# Question 1(e): Calculate μb, the vector of estimated expected values (or “fitted values”). Only print out the first six values.
# calculate μb using: mew = X*betahat

mew = X %*% beta_hat
head(mew)

# Question 1(f): Calculate σb, the estimated error variance.
# calculate σb using: σb^2 = RSS / (n - k - 1)
# where RSS = THETAsum (yi - f(xi))^2

# calculate RSS first
RSS = 0
Yhat = X %*% beta_hat
for (i in 1:nrow(X)) {
  diff_i = Y[i] - Yhat[i]
  RSS = RSS + (diff_i)^2
}

row_hat_sq = RSS/(nrow(X) - ncol(X))

## Not sure about this one

# Question 1(g): Calculate standard errors for the estimated coefficients.
# calculate SE using: SE = row / sqrt(n)

SE = sqrt(row_hat_sq) / (nrow(X)^2)

## Not sure about this one     

# Question 1(h): Calculate t-test statistics and p-values to test individual null hypotheses that the coefficients are equal to zero.

## Not sure about this one   

# Question 1(i): Calculate confidence intervals for the coefficients.