# analysis of data for Robin and Daniel
# 28.3.14

# load libs
library(MASS)

# read data
dat <- read.csv("/Users/danielmoyo/Dropbox/pyabm/simfiles/gls.csv")
attach(dat)

# check data

str(dat)
head(dat)
names(dat)
dim(dat)
(N <- nrow(dat))

 # recode factors as factors

dat$edu<-factor(dat$edu)
dat$income<-factor(dat$income)
dat$gender<-factor(dat$gender)
dat$fdrink<-factor(dat$fdrink)
dat$parenthood<-factor(dat$parenthood)
dat$partnership<-factor(dat$partnership)
dat$paidlabour<-factor(dat$paidlabour)
dat$respagrp<-factor(dat$respagrp)

# [1] "pid"               "respagrp"          "age"              
# [4] "gender"            "edu"               "income"           
# [7] "parenthood"        "partnership"       "paidlabour"       
#[10] "parentpartner"     "parentpaid"        "partnerpaid"      
#[13] "paidpartnerparent" "ca"                "fdrink"           
#[16] "logweekly"         "consumption"      
# 


##################
# model
##################

# linear model


# transform consumption
lambda <- 0.129
trans.cons <- (dat$consumption ^ lambda - 1) / lambda

# append transformed consumption to data.frame
dat <- data.frame(dat, trans.cons)
names(dat)

form <- formula(trans.cons ~ age + I(age^2) + I(age^3) + respagrp + gender + 
    edu + income + fdrink + parenthood + partnership + paidlabour + 
    age:gender + age:income + age:fdrink + gender:fdrink + gender:parenthood + 
    parenthood:partnership + gender:paidlabour + parenthood:paidlabour + 
    partnership:paidlabour + gender:parenthood:paidlabour + parenthood:partnership:paidlabour)

m <- lm(form, data=dat)
summary(m)
summary(m)$sigma^2
sigma <- summary(m)$sigma

#write.csv(coef(m), "coefs.csv")
#write.csv(vcov(m), "variance_matrix.csv")

# some plots
opar <- par(mfrow = c(2, 2))
plot(m)
par(opar)

qqnorm(resid(m))
qqline(resid(m))

lambda
back.tran <- function(x) (lambda * x + 1) ^ (1/lambda)

# simulate 10 datasets

errors <- rnorm(N*10, 0, sigma)

beta.draw <- mvrnorm(10, coef(m), vcov(m))
# t() transposes
sim.mean <- model.matrix(form, data=dat) %*% t(beta.draw)
dim(sim.mean)
sims <- sim.mean + errors
dim(sims)

summary(back.tran(sims))
summary(consumption)

sim.mat <- matrix(back.tran(sims), ncol=10) # place simulated data in a matrix with 10 columns

# plot a histogram with density overlaid

breaks <- 50
hist(consumption, breaks=breaks, prob=TRUE, border="grey", 
	main="Simulations from linear model of transformed consumption")
lines(density(consumption), col=1)
apply(sim.mat, 2, function(x) lines(density(x), col=2, lty=3))
legend(60, 0.03, title="Kernel density", c("observed", "simulated"), col = c(1, 2), lty = c(1,3))



