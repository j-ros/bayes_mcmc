#Q3

#Fit model for PlantGrowth discussed in Lecture8 with different variances for each group.
#Compare estmates between oiginal model and this model

library("rjags")

mod_string0 = " model {    
        for (i in 1:length(y)) {        
                y[i] ~ dnorm(mu[grp[i]], prec)    
        }        
        for (j in 1:3) {        
                mu[j] ~ dnorm(0.0, 1.0/1.0e6)    
        }        
        prec ~ dgamma(5/2.0, 5*1.0/2.0)    
        sig = sqrt( 1.0 / prec ) 
} " 

data_jags0 = list(y=PlantGrowth$weight,               
                 grp=as.numeric(PlantGrowth$group)) 
params0 = c("mu", "sig") 
inits0 = function() {    
        inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0)) 
} 

mod0 = jags.model(textConnection(mod_string0), data=data_jags0, inits=inits0, n.chains=3) 
update(mod0, 1e3) 
mod_sim0 = coda.samples(model=mod0, variable.names=params0, n.iter=5e3)

mod_string1 = " model {
        for (i in 1:length(y)) {
                y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
        }

        for (j in 1:3) {
                mu[j] ~ dnorm(0.0, 1.0/1.0e6)
                prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
        }

        sig = sqrt( 1.0 / prec )
} "

data_jags1 = list(y=PlantGrowth$weight, 
              grp=as.numeric(PlantGrowth$group))

params1 = c("mu", "sig")

inits1 = function() {
    inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod1 = jags.model(textConnection(mod_string1), data=data_jags1, inits=inits1, n.chains=3)
update(mod1, 1e3)

mod_sim1 = coda.samples(model=mod1, variable.names=params1, n.iter=5e3)

summary(mod_sim0)
summary(mod_sim1)

#A: Group 2 posterior mean variance was affected the most by fitting a model
#with independent variances for each group.

#Q4

#Compute difference in dic for both models
dic0 <- dic.samples(mod0, n.iter=1e5)
dic1 <- dic.samples(mod1, n.iter=1e5)
dic0-dic1

#A: -3.89

#Q6

#Compute Highest Posterior Density Interval for mu[3]-mu[1] for model0

mod_csim0 = as.mcmc(do.call(rbind, mod_sim0))
HPDinterval(mod_csim0[,3]-mod_csim0[,1])

#A: (-0.14, 1.13)

