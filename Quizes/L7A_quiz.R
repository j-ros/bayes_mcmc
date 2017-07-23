#Q3

#Variable with strongest linear relationship to per-capita education expenditures

library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables

#A: Income

#Q4

#Model education ~ all other 3 variables using non-informative priors. 
#Report posterior mean estimate of intercept.

mod_lm <- lm(education ~ income + young + urban, data=Anscombe)
summary(mod_lm)

#A: -2.868e+02

#Q6

#Use JAGS to fit given model. Check convergence of chains.

#Define model and intiialize
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

update(mod, 1000) # burn-in
mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5000)

#Diagnose chain convergence
plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

#A: High autocorrelation in coefficients. Run chains longer.

#Q8

#Run residual plot of predicted values vs residuals for the first model.
#Are there any issues?

plot(mod_lm)

#A: residual variance increases as predicted values increase.