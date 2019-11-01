# Regression-Bootstrapping
#############
#### Q2 #####
#############

## a ##

install.packages("arm")
install.packages("Matching")
library(Matching)
library("arm")
data("lalonde")

treated_units_lalonde <- lalonde[which(lalonde$treat != 0), ]
lm_treated_units_lalonde <- lm(re78 ~ age + (age^2) + educ + treat + (treat*age) + re74 + re75, data = treated_units_lalonde)

#simulation
iterations <- 10000
sim_treated_units_lalonde <- sim(lm_treated_units_lalonde, n.sims = iterations)

#predicting re78 for each age holding every coefficient at their means
pred_re78 <- matrix(data = NA, nrow = iterations, ncol = length(min(lalonde$age):max(lalonde$age)))

mean_educ <- mean(treated_units_lalonde$educ)
mean_re74 <- mean(treated_units_lalonde$re74)
mean_re75 <- mean(treated_units_lalonde$re75)

for (age in min(lalonde$age):max(lalonde$age)) {
  A <- c(1, age, (age^2), mean_educ, treated_units_lalonde$treat, (treated_units_lalonde$treat*age), mean_re74, mean_re75)
  for (i in 1:iterations) {
    pred_re78[i, age + 1 - min(lalonde$age)] <- sum(A*sim_treated_units_lalonde@coef[i,])
  }
}

confint_mean <- apply(pred_re78, 2, quantile, probs = c(0.025, 0.975))
table_treated <- t(data.frame(confint_mean))
colnames(table_treated <- c("Lower Bound: Mean 95% Interval", "Upper Bound: Mean 95% Interval"))
  table_treated <- data.frame(table_treated, mean_educ, mean_re74, mean_re75)
  rownames(table_treated) <- min(lalonde$age):max(lalonde$age)
  View(table_treated)

## b ##

## c ##

