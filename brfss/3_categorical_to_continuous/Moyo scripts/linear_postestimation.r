# analysis of data for Robin and Daniel
# 28.3.14

if(FALSE) {
# simultated vs actual consumption
plot.new()
# actual data taken from plothse in dropbox
actual <- c(18.7898,15.32611,19.57846,17.82544,17.82544)
xactual <- c(2003,2005,2006,2008,2009)
#simulated <- c(18.7898,19.36411,18.39654,18.50256,17.95612,17.11673,16.53526)
# following data taken from mean(means) at line 133
simulated <- c(18.7898,19.29381,19.10973,18.9189,18.60882,18.33855,17.99581)

xsimulated <- c(2003,2004,2005,2006,2007,2008,2009)
plot(xactual,actual,ylim=c(0,30),col="blue",ylab="Mean Weekly Units",xlab="Year",lwd=2.0)
par(new=T)
plot(xsimulated,simulated,ylim=c(0,30),col="red",ylab="",xlab="",pch=4,lwd=2.0)
legend(2007, 30, title="", c("observed", "simulated"), col = c("blue", "red"), pch = c(1,4), lwd=2,box.col="white")
grid(NULL,NULL,lwd=2)
#lines(xsimulated,simulated,ylim=c(10,30),ylab="",xlab="",col="red")
#lines(xactual,actual,ylim=c(10,30),ylab="",xlab="",col="blue")
}

# load libs
library(bda)
library(MASS)
library(scales)
library("nlme")
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
#summary(m)
#summary(m)$sigma^2
sigma <- summary(m)$sigma

lambda <- 0.129
back.tran <- function(x) (lambda * x + 1) ^ (1/lambda)
#N <- nrow(dat)

# plot a histogram with density overlaid
#cl <- c(1,2,3,4,5,6,7)
#cl <- c(1,'#cd853f','#b22222','#228b22','#000080','#8a2be2','#000111','#555555')
#cl <- c(1,2,2,2,2,2,2,2,2)
cl <- c('#b22222','#b22222','#b22222','#b22222','#b22222','#b22222','#b22222')
breaks <- 50

#plot(density(consumption), col=1);
par(mfrow=c(2,3))
ohthreeline <- density(dat$consumption)

print("2003 density")
#print(ohthreeline)

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

meansmat <- data.frame()
for (j in 1:10) {
means <- c()
for (i in 2:7) {
  fname <- paste("/Users/danielmoyo/Dropbox/ESRC132127/WP2/ESRC_RA/Data/attitude_scenarios/decreasing/agentTPB_0_200",i+2,".csv",sep='')
  year <- codeData(read.csv(fname))
  yearmean <- predict(m, year)
  errors <- rnorm(nrow(year), 0, sigma)
  sims <- yearmean + errors
  #print(paste("200",i+2," density",sep=''))
  kspvalue <- as.double(ks.test(dat$consumption,back.tran(sims))[2])
  #hist(dat$consumption, breaks=breaks, prob=TRUE, border="grey",xlab="Mean Consumption (units)",main=paste("200",i+2,"\n KS-test P=",round(kspvalue,3),sep=''),lwd=0.5,col=alpha(0.1,0.1))
  means <- c(means,mean(back.tran(sims)))
  #print(mean(means))
  #linedata <- density(back.tran(sims))
  #fit1 <- density(back.tran(sims))
  #fit2 <- replicate(100, { x <- sample(back.tran(sims), replace=TRUE); 
  #                          density(x, from=min(fit1$x), to=max(fit1$x))$y } ) 
  #fit3 <- apply(fit2, 1, quantile, c(0.025,0.975)) 
  #polygon( c(fit1$x, rev(fit1$x)), c(fit3[1,], rev(fit3[2,])), col='grey', border=F) 
  #par(new=T)
  #lines(linedata, col=cl[i-1],lty=1,lwd=2)
  #lines(ohthreeline,col=1,lty=2,lwd=2)
  #legend(60, 0.03, title="Kernel density", c("2003", "simulated"), col = c(1, cl[i-1]), lty = c(3,1), lwd=2,box.col="white")
}
meansmat <- rbind(meansmat,means)
}
names(meansmat) <- c(2004,2005,2006,2007,2008,2009)

#legend(title="Kernel density", c("2003","2004","2005","2006","2007","2008","2009"), col = cl, lty = 1, lwd=3, cex=1.5)
ohthrees <- data.frame()
for (i in 1:1000) {
  ohthrees <- rbind(ohthrees,18.7898)
}
names(ohthrees)[1] <- "2003"
meansmat <- cbind(ohthrees,meansmat)

boxplot(meansmat,col="red",ylim=c(0,30),xlim=c(1,7),pch=1,ylab="",xlab="",lwd=0.5)
par(new=T)
plot(xactual,actual,ylim=c(0,30),col="blue",ylab="Mean Weekly Units",xlab="Year",lwd=2.0,pch=4,cex=1.5)
legend(2007, 30, title="", c("observed", "simulated"), col = c("blue", "black"), pch = c(4,1), lwd=2,box.col="white")
grid(NULL,NULL,lwd=2)
