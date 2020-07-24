# 3_make_Bayes_SMR.R
# Create the Bayesian models of SMR in WinBUGS code
# April 2019

## A. Bayes model of SMR with natural spline basis
bfile = 'R/bayes.ns.txt' # in R folder
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N){
    O[i] ~ dpois(mu[i])
    log(mu[i]) <- log(E[i]) + regress[i]
    regress[i] <- inprod(beta[1:J], X[i, 1:J])
    }
    for (k in 1:J){
    beta[k] ~ dnorm(0, 0.0001)
    }
  }', file=bugs)
close(bugs)

## B. Bayes model of imputing life table
# used log-transform
# simpler version with no change in parameters over time
bfile = 'R/impute.life.txt' # in R folder
bugs = file(bfile, 'w')
cat('model{
  for (i in 1:N) { # age/gender loop
    age.fun[i] <- (beta[2] + beta[3]*gender[i]) * exp((beta[4] + beta[5]*gender[i]) * age[i])
    for (j in 1:M) { # year loop
      L[i, j] ~ dnorm(mu[i, j], tau) 
      log(mu[i, j]) <- beta[1] + age.fun[i] + beta[6]*year[j]
    }
  }
  for (k in 1:J){
    beta[k] ~ dnorm(0, 0.0001)
  }
  tau ~ dgamma(1, 1)
}', file=bugs)
close(bugs)

## B. Bayes model of imputing life table, one per gender
# used log-transform
bfile = 'R/impute.life.txt' # in R folder
bugs = file(bfile, 'w')
cat('model{
  for (i in 1:N) { # age
    for (t in 1:M) { # year loop
      L[i, t] ~ dnorm(mu[i, t], tau) 
      log(mu[i, t]) <- intercept + alpha[t,1] + (alpha[t,2]*age[i]) # log-linear
    }
  }
  # smoothly changing Gompertz parameters over time
  for (t in 1:M){ # years
    for (k in 1:2){ # intercept and slope
      alpha[t,k] <- inprod(gamma[k,1:J], basis.year[t,1:J])
    }
  }
  # hyper-priors
  for (k in 1:2){ # intercept, slope
    for (j in 1:J){ # basis
      gamma[k, j] ~ dnorm(0, 0.001)
    }
  }
  intercept ~ dnorm(0,0.001)
  tau ~ dgamma(1, 1)
}', file=bugs)
close(bugs)


## B. Bayes model of imputing life table
# used log-transform
# simpler version with no change in parameters over time
bfile = 'R/impute.life.txt' # in R folder
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N) { # age/gender loop
    age.fun[i] <- (beta[2] + beta[3]*gender[i]) * exp((beta[4] + beta[5]*gender[i]) * age[i])
    for (j in 1:M) { # year loop
    L[i, j] ~ dnorm(mu[i, j], tau) 
    log(mu[i, j]) <- beta[1] + age.fun[i] + beta[6]*year[j]
    }
    }
    for (k in 1:J){
    beta[k] ~ dnorm(0, 0.0001)
    }
    tau ~ dgamma(1, 1)
    }', file=bugs)
close(bugs)

## B. Bayes model of imputing life table, one per gender
# used log-transform; moving knots
bfile = 'R/impute.life.txt' # in R folder
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N) { # age
    for (t in 1:M) { # year loop
    L[i, t] ~ dnorm(mu[i, t], tau[i]) 
    log(mu[i, t]) <- intercept[t] + (slope[t]*age[i]) + (alpha[3]*step(age[i]-knot[1])*(age[i]-knot[1])) 
    + (slope3[t]*step(age[i]-knot[2])*(age[i]-knot[2])) 
    }
    }
    for (t in 1:M) { # time-varying parameters
    intercept[t] <- alpha[1] + beta[1]*year[t] + gamma[1]*year2[t]
    slope[t] <- alpha[2] + beta[2]*year[t] + gamma[2]*year2[t]
    slope3[t] <- alpha[4] + beta[4]*year[t] + gamma[4]*year2[t] # second knot, first knot was too much
    }
    for (k in 1:4) { 
    beta[k] ~ dnorm(0,0.001)  # linear slopes for time-varying intercept and slope
    gamma[k] ~ dnorm(0,0.001) # quadratic changes for time-varying intercept and slope
    }
    alpha[1] ~ dnorm(0,0.001) # intercept
    alpha[2] ~ dexp(1) # slope must be positive (ageing)
    alpha[3] ~ dexp(1) # slope must be positive (ageing)
    alpha[4] ~ dnorm(0, 0.01)
    for (i in 1:N) { # precision varies by age
    log(tau[i]) <- tau.int + tau.slope*age[i]
    }
    tau.int ~ dnorm(0,0.001)
    tau.slope ~ dnorm(0,0.001)
    knot[1] ~ dunif(0, 0.32) # from 18 to 50 years
    knot[2] ~ dunif(0.33, 0.82) # from 51 to 100 years
    }
', file=bugs)
close(bugs)

