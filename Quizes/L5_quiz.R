#Q7
# posterior mean for y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
# using 5k iterations of gibbs sampling
# In a model with normal likelihood and unknown mean and unknown variance, 
# with a normal prior for the mean and an inverse-gamma prior for the variance.

update_mu = function(n, ybar, sig2, mu_0, sig2_0) {  
        sig2_1 = 1.0 / (n / sig2 + 1.0 / sig2_0)  
        mu_1 = sig2_1 * (n * ybar / sig2 + mu_0 / sig2_0)  
        rnorm(n=1, mean=mu_1, sd=sqrt(sig2_1)) 
}

update_sig2 = function(n, y, mu, nu_0, beta_0) {  
        nu_1 = nu_0 + n / 2.0  
        sumsq = sum( (y - mu)^2 ) # vectorized  
        beta_1 = beta_0 + sumsq / 2.0  
        out_gamma = rgamma(n=1, shape=nu_1, rate=beta_1) # rate for gamma is shape for inv-gamma  
        1.0 / out_gamma # reciprocal of a gamma random variable is distributed inv-gamma 
}

gibbs = function(y, n_iter, init, prior) {  
        ybar = mean(y)  
        n = length(y)    ## initialize  
        mu_out = numeric(n_iter)  
        sig2_out = numeric(n_iter)    
        mu_now = init$mu    ## Gibbs sampler  
        for (i in 1:n_iter) {    
                sig2_now = update_sig2(n=n, y=y, mu=mu_now, nu_0=prior$nu_0, beta_0=prior$beta_0)    
                mu_now = update_mu(n=n, ybar=ybar, sig2=sig2_now, mu_0=prior$mu_0, sig2_0=prior$sig2_0)        
                sig2_out[i] = sig2_now    
                mu_out[i] = mu_now  
        }    
        cbind(mu=mu_out, sig2=sig2_out) 
}

y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
ybar = mean(y) 
n = length(y) 
## prior 
prior = list() 
prior$mu_0 = 0.0 
prior$sig2_0 = 1.0 
prior$n_0 = 2.0 # prior effective sample size for sig2 
prior$s2_0 = 1.0 # prior point estimate for sig2 
prior$nu_0 = prior$n_0 / 2.0 # prior parameter for inverse-gamma 
prior$beta_0 = prior$n_0 * prior$s2_0 / 2.0 # prior parameter for inverse-gamma 

set.seed(53) 
init = list() 
init$mu = 0.0 

post = gibbs(y=y, n_iter=5e3, init=init, prior=prior)

library("coda") 
plot(as.mcmc(post))
summary(as.mcmc(post))

#A: -1

#Q8

#Re run analysis with a prior for mu that is N(1,1)

prior$mu_0 = 1.0
post = gibbs(y=y, n_iter=5e3, init=init, prior=prior)
plot(as.mcmc(post))
summary(as.mcmc(post))

#A: Posterior mean is -0.486 which suggests data strongly favours
# estimating mu to be negative.
