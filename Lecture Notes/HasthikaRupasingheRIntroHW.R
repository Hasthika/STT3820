# Check conditions

p <- 0.07 # Population Proportion of interest
n <- 233 # Sample size 

n*p
n*p > 10
n*(1-p) > 10
n*(1-p)

# Mean and the sd of the sampling distribution for phat
meanphat <- p
meanphat
sdphat<- sqrt(p*(1-p)/n)
sdphat

#4
1-pnorm(0.1, meanphat, sdphat)

#5
pnorm(.05, meanphat, sdphat)

#6
pnorm(.12, meanphat, sdphat) - pnorm(.08, meanphat, sdphat)

#
#qnorm(percentless,mean,sd)
qnorm(0.75,meanphat, sdphat)



dots <- seq(1,6)
dots

probdots <- c(1/6, 1/6, 1/6,1/6, 1/6, 1/6)
probdots

x <- 2
x

vec <- c(2, 3, 4, 6, 7)
vec

dots <- seq(1,6)
dots

probdots <- c(2/6, 0.5/6, 0.5/6, 1/6, 1/6, 1/6 )
probdots
sum(probdots)



#4
S1 <- sample(dots,size=5,replace=T,prob=probdots)
S1









