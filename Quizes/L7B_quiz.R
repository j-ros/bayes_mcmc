#Q3

#Calculate dic for first model of L7A_quiz.

library("car")  # load the 'car' package
data("Anscombe")
library("rjags")

mod_string = " model {
        for (i in 1:length(education)) {
                education[i] ~ dnorm(mu[i], prec)
                mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
        }

        b0 ~ dnorm(0.0, 1.0/1.0e6)
        for (i in 1:3) {
                b[i] ~ dnorm(0.0, 1.0/1.0e6)
        }

        prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        ## Initial guess of variance based on overall
        ## variance of education variable. Uses low prior
        ## effective sample size. Technically, this is not
        ## a true 'prior', but it is not very informative.
        sig2 = 1.0 / prec
        sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)
params = c("b0","b", "sig")
inits = function() {
        inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)

dic.samples(mod, n.iter=1e5)

#A: 486.2

#Q4

#2 alternative models
#1) Remove urban covariate
#2) Remove urban covariate and add income*youth covariate

#Model 1
mod1_string = " model {
        for (i in 1:length(education)) {
                education[i] ~ dnorm(mu[i], prec)
                mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
        }

        b0 ~ dnorm(0.0, 1.0/1.0e6)
        for (i in 1:2) {
                b[i] ~ dnorm(0.0, 1.0/1.0e6)
        }

        prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        ## Initial guess of variance based on overall
        ## variance of education variable. Uses low prior
        ## effective sample size. Technically, this is not
        ## a true 'prior', but it is not very informative.
        sig2 = 1.0 / prec
        sig = sqrt(sig2)
} "

data1_jags = list(education=Anscombe$education, income=Anscombe$income,
                  young=Anscombe$young)

params1 = c("b0","b", "sig")
inits1 = function() {
        inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)

dic.samples(mod1, n.iter=1e5)

#Model 2
mod2_string = " model {
        for (i in 1:length(education)) {
               education[i] ~ dnorm(mu[i], prec)
                mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
        }

        b0 ~ dnorm(0.0, 1.0/1.0e6)
        for (i in 1:3) {
                b[i] ~ dnorm(0.0, 1.0/1.0e6)
        }

        prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        ## Initial guess of variance based on overall
        ## variance of education variable. Uses low prior
        ## effective sample size. Technically, this is not
        ## a true 'prior', but it is not very informative.
        sig2 = 1.0 / prec
        sig = sqrt(sig2)
} "

data2_jags = list(education=Anscombe$education, income=Anscombe$income,
                  young=Anscombe$young)

params2 = c("b0","b", "sig")
inits2 = function() {
        inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)

dic.samples(mod2, n.iter=1e5)

#A: first model has lower DIC

#Q5

#Using selected model in Q4 (mod), report posterior probability that coefficient
#for income is positive.

update(mod, 1000) # burn-in
mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5000)

mod_csim = as.mcmc(do.call(rbind, mod_sim)) 

colSums(mod_csim>0)

#A: 1

