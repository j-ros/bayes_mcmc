#Q5

#Parameter theta follows a Beta(5,2) in the posterior
#Compute E(theta/(1-theta))
m <- 10000
theta <- rbeta(m,5,2)
mean(theta/(1-theta))
#A: 4.882178

#Q6

#Compute P(theta/(1-theta)>1) on same problem as before
mean(theta/(1-theta)>1)
#A: 0.8897

#Q7

#Compute 0.3 quantile of N(0,1) using MC
quantile(rnorm(m),probs=0.3)
#A: -0.5131792
#qnorm(0.3); -0.5244005
