# load libs
library(bda)
library(MASS)
library(scales)
# read data
dat <- read.csv("/Users/danielmoyo/Dropbox/simfiles/gls.csv")
dat$glfid <- factor(dat$glfid)
# consumption will only be fitted based on the retained individuals in 2002
retained <- read.csv("/Users/danielmoyo/Dropbox/ESRC132127/WP2/ESRC_RA/Data/attitude_scenarios/decreasing/glfretained0.csv")
colnames(retained) <- c("glfid")
retained <- factor(retained$glfid)
dat <- dat[dat$glfid %in% retained, ]

# [1] "pid"               "respagrp"          "age"              
# [4] "gender"            "edu"               "income"           
# [7] "parenthood"        "partnership"       "paidlabour"       
#[10] "parentpartner"     "parentpaid"        "partnerpaid"      
#[13] "paidpartnerparent" "ca"                "fdrink"           
#[16] "logweekly"         "consumption"      
# 

##################
# model function
##################

codeData <- function(data) {
  (N <- nrow(data))
  # recode factors as factors
  data$edu<-factor(data$edu)
  data$income<-factor(data$income)
  data$gender<-factor(data$gender)
  data$fdrink<-factor(data$fdrink)
  data$parenthood<-factor(data$parenthood)
  data$partnership<-factor(data$partnership)
  data$paidlabour<-factor(data$paidlabour)
  data$respagrp<-factor(data$respagrp)
  #names(data)
  return(data)
}

fitConsumption <- function(data) {
  form <- formula(trans.cons ~ age + I(age^2) + I(age^3) + respagrp + gender + 
                    edu + income + fdrink + parenthood + partnership + paidlabour + 
                    age:gender + age:income + age:fdrink + gender:fdrink + gender:parenthood + 
                    parenthood:partnership + gender:paidlabour + parenthood:paidlabour + 
                    partnership:paidlabour + gender:parenthood:paidlabour + parenthood:partnership:paidlabour)
  
  return(form)
}

# 2003 linear model
dat$edu<-factor(dat$edu)
dat$income<-factor(dat$income)
dat$gender<-factor(dat$gender)
dat$fdrink<- dat$fdrink + 1 # fdrink is recoded in simulation output
dat$fdrink<-factor(dat$fdrink)
dat$parenthood<-factor(dat$parenthood)
dat$partnership<-factor(dat$partnership)
dat$paidlabour<-factor(dat$paidlabour)
dat$respagrp<-factor(dat$respagrp)
# transform consumption
lambda <- 0.129
trans.cons <- (dat$consumption ^ lambda - 1) / lambda
# append transformed consumption to data.frame
dat <- data.frame(dat, trans.cons)

m <- lm(fitConsumption(dat), data=dat)
sigma <- summary(m)$sigma

lambda <- 0.129
back.tran <- function(x) (lambda * x + 1) ^ (1/lambda)
cl <- c('#b22222','#b22222','#b22222','#b22222','#b22222','#b22222','#b22222')
breaks <- 50
par(mfrow=c(2,3))
ohthreeline <- density(dat$consumption)

bootConsumption <- function(simulatedConsumption) {
  
  fits <- c(fit1,fit2,fit3)
  return(fits)
}



fname <- "/Users/danielmoyo/Dropbox/ESRC132127/WP2/ESRC_RA/Data/attitude_scenarios/decreasing/agentTPB_0_2003.csv"
year <- codeData(read.csv(fname))
yearmean <- predict(m, year)
errors <- rnorm(nrow(year), 0, sigma)
sims <- yearmean + errors
print(paste("2003"," density",sep=''))
#print(density(back.tran(sims)))
print(ks.test(dat$consumption,back.tran(sims)))
write.csv(sims, "/Users/danielmoyo/Desktop/sims.csv")
write.csv(dat$consumption, "/Users/danielmoyo/Desktop/consumption.csv")

for (i in 2:7) {
  fname <- paste("/Users/danielmoyo/Dropbox/ESRC132127/WP2/ESRC_RA/Data/attitude_scenarios/decreasing/agentTPB_0_200",i+2,".csv",sep='')
  year <- codeData(read.csv(fname))
  yearmean <- predict(m, year)
  errors <- rnorm(nrow(year), 0, sigma)
  sims <- yearmean + errors
  hist(dat$consumption, breaks=breaks, prob=TRUE, border="grey",xlab="Mean Consumption (units)",main=paste("200",i+2,sep=''),lwd=0.5,col=alpha(0.1,0.1))
  fit1 <- density(back.tran(sims))
  fit2 <- replicate(1000, { x <- sample(back.tran(sims), replace=TRUE); 
                            density(x, from=min(fit1$x), to=max(fit1$x))$y } ) 
  fit3 <- apply(fit2, 1, quantile, c(0.025,0.975)) 
  polygon( c(fit1$x, rev(fit1$x)), c(fit3[1,], rev(fit3[2,])), col='grey', border=F) 
  par(new=T)

  lines(ohthreeline,col=1,lty=2,lwd=2)

  legend(60, 0.03, title="Kernel density", c("2003", "simulated"), col = c(1, cl[i-1]), lty = c(3,1), lwd=2,box.col="white")
}
