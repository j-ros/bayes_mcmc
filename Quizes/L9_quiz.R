#Q2

#Identify which of the four variables has weakest association with prob of success.

library("MASS")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )

#A: OME

#Q3

#Fit a reference model with non informative priors. 
#Report the posterior mode estimate of the coefficient for low OME

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)
plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)

#A: -0.237150

#Q4

#Report characteristics of baseline group ignoring Age and Loud variables.

X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)

#A: children with high OME and coherent noise.

#Q5

#Fit JAGS model with fairly uninformative priors.
library("rjags")

mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}
	
	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}
	
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).

params = c("b0", "b") 

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3) 
update(mod, 1e3) 
mod_sim = coda.samples(model=mod, variable.names=params, n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

#First check the Raftery-Lewis diagnostic, what does it show?

raftery.diag(mod_sim)

#A: large dependecy factor for some variables, we need longer chains to
#produce reliable intervals.

#Q7

#Calculate point estimate for the probability of correct response for a child
#with age 60 months, high OME, using coherent stimulus of 50 decibels.

pm_coef <- colMeans(mod_csim)
pm_Xval <- pm_coef[5]+pm_coef[1]*60 + pm_coef[2]*0 + pm_coef[3]*50 + pm_coef[4]*0
1/(1+exp(-pm_Xval))

#A: 0.92

#Q8

#Calculate vector of probabilities for each observation in dataset
pm_Xb <- pm_coef[5] + X %*% pm_coef[1:4]
phat <- 1/(1+exp(-pm_Xb))

#Now compute proportion of in-sample observations that are correclt classified,
#following the criteria that both model prediction and observed correct response 
#rate are higher than 0.7 or both lower.

(tab0.7 = table(phat > 0.7, (dat$Correct / dat$Trials) > 0.7))
sum(diag(tab0.7)) / sum(tab0.7)

#A: 0.84