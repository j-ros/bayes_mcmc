#Q3
#Assume transition matrix T=[0,1;0.3,0.7] where first row means player 1 not playing 
#and second row player 1 playing.
#Compute prob that player 1 is playing in Game 4 if not playing Game 1.
#State (1,0) means player not playing, state (0,1) means player playing
t = matrix(c(0,1,0.3,0.7),nrow=2, byrow=T)
c(1,0) %*% (t %*% t %*% t %*% t)
#A: 0.763

#Q4
#Stationary distribution for example Q3

n <- 5000
x <- numeric(n)
x[1] <- 1 #start at state 0 (not playing), this is row 1 of t
for( i in 2:n ){
        x[i] <- sample.int(2, size = 1, prob=t[x[i-1],])
}
table(x)/n
#A: 0.227 (not playing) 0.773 (playing)

#Q5
#If players draw from stationary distribbution on Q4 to select if player plays
#on Game 1. Find probability he plays Game 4
c(0.227,0.773) %*% (t %*% t %*% t %*% t)
#A: 0.2307387 (not playing) 0.7692613 (playing). 
#Same as Q4 answer since it's stationary distribution (except rounding error)