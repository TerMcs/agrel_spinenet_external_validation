library(irr)

rate1 <- 0.2118 # the probability that the first rater will record a positive diagnosis
rate2 <- 0.1908 # the probability that the second rater will record a positive diagnosis
k1 <- 0.548858 # the true Cohen's Kappa statistic
k0 <- 0.39 # the value of kappa under the null hypothesis

mc <- N.cohen.kappa(rate1, rate2, k1, k0, alpha=0.05, 
              power=0.8, twosided=F)
mc

rate1 <- 0.5675
rate2 <- 0.4957
k1 <- 0.51
k0 <- 0.39

dd<- N.cohen.kappa(rate1, rate2, k1, k0, alpha=0.05, 
              power=0.8, twosided=F)
dd
