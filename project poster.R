library("tidyverse")
library("ggplot2")
library("readr")
library("lubridate")
library("zoo")
library(lmtest)
library(stats)
library(vars)

#load the dataset cpi,investment rate, loan amount percentage change , mortgage rate
cpi <- read.csv("cpi.csv")
ivr <- read.csv("change of interest.csv")
loan <- read.csv("RELOAN.csv")
mgr <- read.csv("mortgage rate.csv")

cpi <- cpi[-c(482:526),]
mgr <- mgr[-c(482:527),]

#plot the data
colnames(cpi)[2] <- 'cpi'
colnames(ivr)[2] <- 'ivr'
colnames(loan)[2] <- 'loan'
colnames(mgr)[2] <- 'mgr'

cpi$DATE <- ymd(cpi$DATE)
ivr$DATE <- ymd(ivr$DATE)
loan$DATE <- ymd(loan$DATE)
mgr$DATE <- ymd(mgr$DATE)


#test for unit root
library(fUnitRoots)
adfTest(cpi$cpi, lags = 10, type = "c")
adfTest(ivr$ivr, lags = 10, type = "c")
adfTest(loan$loan, lags = 10, type = "c")
adfTest(mgr$mgr, lags = 10, type = "c")

#notice that the p-value of loan is high,it may have unit root,then i decided 
#to take difference of loan
loan$loan <- c(NA, 100*diff(loan$loan)/loan$loan[1:nrow(loan)-1])
adfTest(loan$loan)
loan <- loan %>%
  na.omit()



##Declare  dataset as time series dataset
my_data <- cbind(cpi,ivr[2],loan[2],mgr[2])

TS_Data <- ts(my_data[, -1], start = c(1980, 1), frequency = 12)

#plot 4 time series variables
par(mar=c(5,4,5,4)+ 0.3)
plot(my_data$DATE,my_data$ivr,type = "l",lty =1,xlab="DATE",ylab="Investment rate")
par(new = TRUE)
plot(my_data$DATE,my_data$loan,type="l",lty = 3,axes=FALSE,xlab="",ylab="")
axis(side=4,at =pretty(range(my_data$loan)))
mtext("LOAN",side = 4)
legend(x="topleft",legend=c("IVR % left","LOAN % right"),lty=c(1,3),bty="n")


par(mar=c(5,4,5,4)+ 0.3)
plot(my_data$DATE,my_data$mgr,type = "l",lty =1,xlab="DATE",ylab="Mortgage rate")
par(new = TRUE)
plot(my_data$DATE,my_data$cpi,type="l",lty = 3,axes=FALSE,xlab="",ylab="")
axis(side=4,at =pretty(range(my_data$cpi)))
mtext("CPI",side = 4)
legend(x="top",legend=c("MGR % left","CPI % right"),lty=c(1,3),bty="n")



#Run VAR
VARselect(my_data[c("cpi","ivr","loan","mgr")],lag.max = 10)$selection
# SC(n)=2= BIC

Model1 <- VAR(TS_Data[, c("loan", "cpi", "ivr","mgr")], p = 2, type = "const")

#test for white noise.
model1_residuals <- residuals(Model1)
m1 <- lm(model1_residuals~1)
dw_test <- dwtest(m1)
dw_test
# DW =1.91, residuals of VAR  are stationary white noises.

# make a restrict assumption for VAR
amat <- diag(4)
amat[2,1] <- NA 
amat[3,1] <- NA 
amat[3,2] <- NA
amat[4,1] <- NA
amat[4,2] <- NA
amat[4,3] <- NA

SVARMod1 <- SVAR(Model1, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("scoring", "direct"))
SVARMod1


#Get all 16 IRFs
ir=irf(SVARMod1)
plot(ir)

#get the specific irfs of this model
SVAR1 <- irf(SVARMod1, impulse = "loan", response = "loan") 
plot(SVAR1)
SVAR1_1 <- irf(SVARMod1, impulse = "loan", response = "ivr") 
plot(SVAR1_1)

SVAR2 <- irf(SVARMod1, impulse = "loan", response = "cpi") 
plot(SVAR2)

SVAR3 <- irf(SVARMod1, impulse = "loan", response = "mgr") 
plot(SVAR3)

SVAR4 <- irf(SVARMod1, impulse = "cpi", response = "loan") 
plot(SVAR4)

SVAR5 <- irf(SVARMod1, impulse = "cpi", response = "cpi") 
plot(SVAR5)

SVAR6 <- irf(SVARMod1, impulse = "cpi", response = "ivr") 
plot(SVAR6)

SVAR7 <- irf(SVARMod1, impulse = "cpi", response = "mgr") 
plot(SVAR7)

SVAR8 <- irf(SVARMod1, impulse = "ivr", response = "loan") 
plot(SVAR8)

SVAR9 <- irf(SVARMod1, impulse = "ivr", response = "cpi") 
plot(SVAR9)

SVAR10 <- irf(SVARMod1, impulse = "ivr", response = "ivr") 
plot(SVAR10)

SVAR11 <- irf(SVARMod1, impulse = "ivr", response = "mgr") 
plot(SVAR11)

SVAR12 <- irf(SVARMod1, impulse = "mgr", response = "ivr") 
plot(SVAR12)

SVAR13 <- irf(SVARMod1, impulse = "mgr", response = "mgr") 
plot(SVAR13)
SVAR14 <- irf(SVARMod1, impulse = "mgr", response = "loan") 
plot(SVAR14)
SVAR15 <- irf(SVARMod1, impulse = "mgr", response = "cpi") 
plot(SVAR15)
# robust test
Model2 <- VAR(TS_Data[, c("loan", "ivr", "cpi","mgr")], p = 2, type = "const")
SVARMod2 <- SVAR(Model2, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod = c("scoring", "direct"))
#get the specific irfs of this model
SVARn1 <- irf(SVARMod2, impulse = "loan", response = "loan") 
plot(SVARn1)
# significant
SVARn2 <- irf(SVARMod2, impulse = "loan", response = "ivr") 
plot(SVARn2)
# not significant
SVARn3 <- irf(SVARMod2, impulse = "loan", response = "cpi") 
plot(SVARn3)
# not significant
SVARn4 <- irf(SVARMod2, impulse = "loan", response = "mgr") 
plot(SVARn4)
# significant for 3 month
SVARn5 <- irf(SVARMod2, impulse = "cpi", response = "loan") 
plot(SVARn5)
# no significant
SVARn6 <- irf(SVARMod2, impulse = "cpi", response = "cpi") 
plot(SVARn6)
# significant
SVARn7 <- irf(SVARMod2, impulse = "cpi", response = "ivr") 
plot(SVARn7)
# not significant

SVARn8 <- irf(SVARMod2, impulse = "cpi", response = "mgr") 
plot(SVARn8)
#  delay 5 months significant and disapper 3 month later

SVARn9 <- irf(SVARMod2, impulse = "ivr", response = "loan") 
plot(SVARn9)
# not significant
SVARn10 <- irf(SVARMod2, impulse = "ivr", response = "cpi") 
plot(SVARn10)
# not significant

SVARn11 <- irf(SVARMod2, impulse = "ivr", response = "ivr") 
plot(SVARn11)
# significant for 6 months
SVARn12 <- irf(SVARMod2, impulse = "ivr", response = "mgr") 
plot(SVARn12)
# not significant
SVARn13 <- irf(SVARMod2, impulse = "mgr", response = "ivr") 
plot(SVARn13)
#not significant

SVARn14 <- irf(SVARMod2, impulse = "mgr", response = "mgr") 
plot(SVARn14)
# significant and persistent

SVARn15 <- irf(SVARMod2, impulse = "mgr", response = "loan") 
plot(SVARn15)
# no significant

SVARn16 <- irf(SVARMod2, impulse = "mgr", response = "cpi") 
plot(SVARn16)
# contemperous 

