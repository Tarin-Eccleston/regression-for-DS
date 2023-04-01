# Diagnostics is used to identify problems with the data and problems with the fitted model

# Outliers
# ________________________
# previously, we just removed outliers, there's a BETTER way to deal with it...
# outliers or extreme values can be used as a worst case
# outliers can also drive the line of best fit, which is not useful or accurate
# the way we deal with outliers depends on the context of the analysis

# Note: can have a go at question 1 in ass 2 after W4F lesson

# Detecting High Leverage Points
# ________________________
# use the H (hat matrix), and look at ith diagonals to measure the influence and
# leverage of the ith observation
# if hij goes to 1, then all of hij goes to 0
# therefore large values of hij indicates points where the observation (yi) has
# a large influence on the fitted values (mew_hat_i)
# hat matrix can esp be useful when you have many explanatory variables where we can't
# see high leverage points that easily between variables

setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/diagnostics-model-building")

catheter_df = read.table("data/catheter.data", header = TRUE)

# get hat matrix
catheter_fit = lm(ca ~ ht, data = catheter_df)
hmds = influence(catheter_fit)$hat

# hat values only depend on x values. two datasets with the same x values BUT different
# y values will show the same h values

# ok we need to catchup lol

