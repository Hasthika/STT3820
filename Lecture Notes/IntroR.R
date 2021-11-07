heat2 <- c(105.9,106.4,106.4,106.5,107,108.8,109.6,115.3)
heat7 <- c(106.1,106.1,106.4,106.9,108.4,108.5,117.5)

ybar1 <- mean(heat2) #27.69
s1 <- sd(heat2) #4.02
n1 <- length(heat2)#33

ybar2 <- mean(heat7)#26.66
s2 <- sd(heat7) #2.86
n2 <- length(heat7)#34

df <- (s1^2/n1 +s2^2/n2)^2/(1/(n1-1)*(s1^2/n1)^2 + 1/(n2-1)*(s2^2/n2)^2)

cat("Degees of freedom is", df, "\n")

tstat <- (ybar1-ybar2)/sqrt(s1^2/n1 +s2^2/n2)

cat("The t-stat is", tstat, "\n")

#less
LTPval<- pt(tstat, df)

#more
GTPval<- 1-pt(tstat, df)

#not equal
NEPval<-2*pt(-abs(tstat), df)


list(LessPval=LTPval,GreaterPval=GTPval,NotEqualPval=NEPval)




ybar1 <- 28.1
s1 <- 3.93
n1 <- 31

ybar2 <- 25.98
s2 <- 2.79
n2 <- 32

df <- (s1^2/n1 +s2^2/n2)^2/(1/(n1-1)*(s1^2/n1)^2 + 1/(n2-1)*(s2^2/n2)^2)

CIlev <- .95

alpha <- 1-CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1- alpha/2
tstar <- qt(oneminusalphaovertwo,df)

tstar  #tstar in a CI formula

LowP <- (ybar1-ybar2) - tstar * sqrt(s1^2/n1 +s2^2/n2)
UpP <- ybar1-ybar2 + tstar * sqrt(s1^2/n1 +s2^2/n2)

CI <- c(LowP,UpP)

cat("The ", CIlev*100, " % confidence interval for mu is", CI)


