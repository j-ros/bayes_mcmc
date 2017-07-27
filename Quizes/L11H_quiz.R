#Q1

#Refit model from L10_quiz. Probability that subject with 29y and from group2
#calls at least 3 times in 30 day period of activity.

dat = read.csv(file="L10_callers.csv", header=TRUE)

library("rjags")

mod_string = " model {
        for (i in 1:length(calls)) {
                calls[i] ~ dpois( days_active[i] * lam[i] )
                log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
        }

        b0 ~ dnorm(0.0, 1.0/1e2)
        b[1] ~ dnorm(0.0, 1.0/1e2)
        b[2] ~ dnorm(0.0, 1.0/1e2)
} "

data_jags = as.list(dat)

params = c("b0", "b")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

pm_params = colMeans(mod_csim)

loglam = pm_params['b0'] + pm_params[c(1,2)] %*% c(29,1)
lam = exp(loglam)
y = rpois(n=1000, lambda=lam*30)
mean(y>=3)

#A:0.238