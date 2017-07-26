dat = read.csv(file="L11A_pctgrowth.csv", header=TRUE)

#Fit hierarchical model to the data in file above
#y_i|theta_{g_i} ~ N(theta_{g_i},sig^2) i=1..53, g_i=1..5
#theta_{g_i}|mu,tau^2 ~ N(mu,tau^2) g=1..5
#mu ~ N(0,1e6); tau^2 ~ IG(1/2,1*3/2); sig^2 ~ IG(2/2,2*1/2)

library("rjags")

mod_string = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(theta[grp[i]],1/(sig2))
        }
        for (j in 1:max(grp)) {
                theta[j] ~ dnorm(mu,1/(tau2))
        }
        mu ~ dnorm(0,1/(1e6))
        invtau2 ~ dgamma(1/2,1*3/2)
        invsig2 ~ dgamma(2/2,2*1/2)

        tau2 = 1/invtau2
        sig2 = 1/invsig2
} "

data_jags = as.list(dat)
params = c("theta", "mu", "tau2", "sig2")
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod,1e3)
mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim,ask=TRUE)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

#Check residuals

pm_params = colMeans(mod_csim)

#Observational level

thetas = pm_params[4:8]
resid = dat$y - thetas[dat$grp]
plot(resid)
plot(yhat[dat$grp],resid)

#Group level residuals

theta_resid = thetas - pm_params['mu']
plot(theta_resid)

#Results: compare posterior means for thetas to non-hierarchical model (ANOVA cell means)
#We can approximate the posterior estimates for the five industry means under a 
#noninformative by simply calculating the sample mean growth for the five industries.
summary(mod_sim)

means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)
plot(means_anova)
points(thetas, col="red")

#A: means from hierarchical model have less variability indicated by less magnitude.