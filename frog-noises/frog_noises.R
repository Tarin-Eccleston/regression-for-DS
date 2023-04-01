setwd("/Users/tarineccleston/Documents/Masters/STATS 762/regression-for-DS/frog-noises")

# Q1a)

frog_df = read.csv("data/frogs.csv")

# find the absolute distance between the frog and each microphone
frog_df$dist = sqrt((abs(frog_df$mic.x - frog_df$animal.x)^2) + (abs(frog_df$mic.y - frog_df$animal.y)^2))
# get the probability of that particular microphone detected that particular frog
# based on the number of calls the frog makes and the detected amount from the mic
frog_df$prob = frog_df$detected / frog_df$n.calls

plot(prob ~ dist, data =- frog_df)
