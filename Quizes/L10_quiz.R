#Q2

#Compute DIC for original model for doctor visits vs model without interaction terms

library("COUNT")
data("badhealth")

library("rjags")

mod_string0 = " model {
        for (i in 1:length(numvisit)) {
                numvisit[i] ~ dpois(lam[i])
                log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
        }

        int ~ dnorm(0.0, 1.0/1e6)
        b_badh ~ dnorm(0.0, 1.0/1e4)
        b_age ~ dnorm(0.0, 1.0/1e4)
        b_intx ~ dnorm(0.0, 1.0/1e4)
} "

data_jags = as.list(badhealth)

params0 = c("int", "b_badh", "b_age", "b_intx")

mod0 = jags.model(textConnection(mod_string0), data=data_jags, n.chains=3)

mod_string1 = " model {
        for (i in 1:length(numvisit)) {
                numvisit[i] ~ dpois(lam[i])
                log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
        }

        int ~ dnorm(0.0, 1.0/1e6)
        b_badh ~ dnorm(0.0, 1.0/1e4)
        b_age ~ dnorm(0.0, 1.0/1e4)
} "

params1 = c("int", "b_badh", "b_age")

mod1 = jags.model(textConnection(mod_string1), data=data_jags, n.chains=3)

## compute DIC
dic0 = dic.samples(mod0, n.iter=1e3)
dic1 = dic.samples(mod1, n.iter=1e3)

#A: mod0 has lower dic than dic1

#Q5

#Read in L10_callers file containing data for 224 customers (24 of which from group2)
#during 90 days. Which plot is most informative to assess the hypothesis that
#callers from group2 call at a higher rate?

dat = read.csv(file="L10_callers.csv", header=TRUE)

plot(dat$calls/dat$days_active ~ dat$isgroup2)

#A: calls/days_active vs isgroup2

#Q7

#Fit poisson model for data in Q5 using N(0,10) as prior for parameters.
#Check convergence and residuals.
#Posterior probability that coeff for indicator isgroup2 is greater than 0?

mod_string2 = " model {
        for (i in 1:length(calls)) {
		calls[i] ~ dpois( days_active[i] * lam[i] )
                log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
        }

        b0 ~ dnorm(0.0, 1.0/1e2)
        b[1] ~ dnorm(0.0, 1.0/1e2)
        b[2] ~ dnorm(0.0, 1.0/1e2)
} "

data_jags2 = as.list(dat)

params2 = c("b0", "b")

mod2 = jags.model(textConnection(mod_string2), data=data_jags2, n.chains=3)

update(mod2, 1e3)

mod_sim2 = coda.samples(model=mod2,
                       variable.names=params2,
                       n.iter=5e3)
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))

## convergence diagnostics
plot(mod_sim2)

gelman.diag(mod_sim2)
autocorr.diag(mod_sim2)
autocorr.plot(mod_sim2)
effectiveSize(mod_sim2)

## compute DIC
dic2 = dic.samples(mod2, n.iter=1e3)

#Check residuals
X = as.matrix(dat[,-c(1,2)])
(pmed_coef = apply(mod_csim2, 2, median))
llam_hat = pmed_coef["b0"] + X %*% pmed_coef[c(2,1)]
lam_hat = exp(llam_hat)
resid = dat$calls - lam_hat*dat$days_active

plot(resid)
plot(lam_hat*dat$days_active, dat$calls)

plot(lam_hat[which(dat$isgroup2 == 0)]*dat$days_active[which(dat$isgroup2 == 0)],
     resid[which(dat$isgroup2 == 0)], xlim=c(0, 6), ylab="residuals", xlab=expression(hat(lambda)), ylim=range(resid))
points(lam_hat[which(dat$isgroup2 == 1)]*dat$days_active[which(dat$isgroup2 == 1)],
       resid[which(dat$isgroup2 == 1)], col="red")

#Prob beta2 >0
mean(mod_csim2[,2]>0)

#A: 1