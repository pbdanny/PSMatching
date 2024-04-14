# Reference : https://dept.stat.lsa.umich.edu/~bbh/hansenKlopfer2006.pdf
# Reference : https://ngreifer.github.io/blog/matching-weights/

library(MatchIt)
library(dplyr)

wt <- c(0,0,0,0, 4.4, 6.1, 0,0,0, 4.4, 5.0, 5.7, 5.9, 6.0, 6.3)
n <- c(LETTERS[seq(1, 6)], LETTERS[seq(18, 26)])
A <- c(rep(1, 6), rep(0, 9))

d <- data.frame(score = wt, name = n, ab_flag = A)
d

# For 1:1 matching (gready & nearnest)----
# stratum propensity score of each unit in the stratum i = n(of treated units) / n(all units) in those stratum i
# since 1:1 then stratum propensity score (e*_i) for treated and control = 1/2
mF <- matchit(A ~ wt, data = d, method = "nearest", estimand = "ATT", replace = FALSE)
mF
md <- match.data(mF)
md

# For matching weight from MatchIt `weights` (used for estimating treatment effect) each treat unit received value = 1
# for control units = e*_i/(1-e*_i) = 1/2 / (1 - 1/2) = 1
md %>% group_by(subclass) %>% summarise(n = n(), tt_weight = sum(weights))
for (i in 1:5) {
  print(subset(md, subclass == i))
}

# For full matching (full)----
# stratum propensity score of each unit in the stratum i = n(of treated units) / n(all units) in those stratum i
# Ex. stratum with 2 treated units matched 1 control unit 
# then stratum propensity score (e*_i) for treated and control = (2)/(2+1) = 2/3

mF <- matchit(A ~ wt, data = d, method = "full", estimand = "ATT", replace = FALSE)
mF
md <- match.data(mF)
md

# For matching weight from MatchIt `weights` (used for estimating treatment effect) each treat unit received value = 1
# for control units = e*_i/(1-e*_i) * Scaling Factor

# Scaling Factor - based on Marginal Mean Weighting through Stratification (MMWS)
# Scaling factor for ATT = 1 - P(A = 1) / P (A = 1)
# P(A = 1) is the overall proportion of treated units vs total units
p_a_1 <- nrow(subset(md, ab_flag == 1)) / nrow(md)
p_a_1

scale_factor <- (1 - p_a_1)/p_a_1
scale_factor

# Then for stratum with 2 treated , 1 control. The weight for
# for control units = e*_i/(1-e*_i) * Scaling Factor = 2/3 / (1-2/3) * 1.5

for (i in 1:5) {
  print(subset(md, subclass == i))
} 

# Or looks another way. The total propotion of treated : control = 6 : 9 = 2 : 3
# But in each stratum proportion not follow, then if we assigned treated weighted = 1
# We have to scale control weight to preserved overall proportion 2 : 3
# If stratum have 2 treated , 1 control the (before scaled) stratum proportion = 2 : 1
# If assigned weight to treate = 1 each, control must have weight = 3 then those
# stratum proportion 2 : 3
# If stratum have 1 treated, 2 control units the (before scaled) stratum proportion = 1 : 2
# The control units each mush have weights = (3/2)/2 = 0.75 each then the statum weight = 1 : (3/4 + 3/4)
# 1 : 3/2 -> 2 : 3
