#Q2

#Fit a model with Laplace prior with variance 2 for each of the 3 coeff
#and an inverse gamma with effective sample size of 1 and prior guess 1
#for the observation vaiance. Compare results with model from L7B_quiz.

library("car")
data("Anscombe")
head(Anscombe)
?Anscombe

Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)

data_jags = as.list(data.frame(Xc))

library("rjags")

mod_string = " model {
        for (i in 1:length(education)) {
                education[i] ~ dnorm(mu[i], prec)
                mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
        }

        b0 ~ dnorm(0.0, 1.0/1.0e6)
        for (i in 1:3) {
                b[i] ~ ddexp(0.0, 1.0/2)
        }

        prec ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
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
summary(mod_sim)

#A: Inferences look similar, b[1] and b[2] are significantly positive 
#and b[2] significantly negative

#Q5

#Refit warp break model with a separate variance for each group,
#using an inverse gamma(1/2,1/2) prior, corresponding to prior sample 1 and
#prior guess 1 for each variance. Report DIC value.

data("warpbreaks")
mod_string = " model {    
        for( i in 1:length(y)) {        
                y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec[woolGrp[i], tensGrp[i]])    
        }        
        for (j in 1:max(woolGrp)) {        
                for (k in 1:max(tensGrp)) {            
                        mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
                        prec[j,k] ~ dgamma(1/2.0, 1*1.0/2.0)
                }    
        }        
        sig = sqrt(1.0 / prec) 
} " 
data_jags = list(y=log(warpbreaks$breaks), woolGrp=as.numeric(warpbreaks$wool), tensGrp=as.numeric(warpbreaks$tension)) 
params = c("mu", "sig") 
mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3) 

dic.samples(mod, n.iter=1e5)

#A: 74.83