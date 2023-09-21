install.packages("tidyverse")
install.packages("GGally")
rm(list=ls())
library(tidyr)
library(tidyverse)
library(ggplot2)
library(GGally)
#question-1
#1.
set.seed(1234)
x1 <- runif(4000,0,1)
x2 <- runif(4000,0,1)
#2.
e <- rnorm(4000,mean=0,sd=1)

y <- c()
for (i in 1:length(x1)){
  y[i]<-x1[i] - 2*x2[i] + e[i]
}

#3.

X <- cbind(x1,x2)
XTX <- t(X)%*%X
XPy <- t(X)%*%y
beta_hat_ols <- solve(XTX)%*%XPy
beta_hat_ols

#4.
residuals <- y - X%*%beta_hat_ols
sigma_squared <- sum(residuals^2)/4000
Avar <- sigma_squared * solve(XTX)
Avar

#5.
t_beta1 <- beta_hat_ols[1]/Avar[1,1]^0.5
t_beta1

#6.
df <- data.frame(y,x1,x2)
df2 <- df[sample(1:nrow(df),4000,replace = T),]

#7.
results <- lm(y~ x1+x2,data=df2)
summary(results)

beta_hat_df2 <- results$coefficients[2:3]
beta_hat_df2

#8.
beta_hat_new <- matrix(NA, nrow = 1000, ncol = 2)
for(i in 1:1000){
  set.seed(i) #set seed to reproduce results
  new_samples <- sample(1:nrow(df),4000,replace = T)
  df3 <- df[new_samples, ]
  y3 <- df3$y
  x3 <- data.matrix(df3[,c('x1','x2')])
  XTX3 <- t(x3)%*%x3
  XPy3 <- t(x3)%*%y3
  beta_hat_ols3 <- solve(XTX3)%*%XPy3
  beta_hat_new[i, ] <- beta_hat_ols3
}

std_deviation <- apply(beta_hat_new,2,sd)
std_deviation

#conclusion: the bootstrapped samples have smaller std value of beta, as shown in results
#0.04 vs 0.05

#question-2
nls_80 <- read.csv("nls80.csv")

#1.
Scatter_Matrix <- ggpairs(nls_80, columns = c("wage","educ","iq"),
                          title = "Scatter Plot Matrix for nls_80 Dataset",
                          axisLabels = "show")
Scatter_Matrix
#we can see iq and educ have positive correlation with wage,the correlation
#between iq and educ have stronger correlation than others. the iq is normal 
#distribution which mean is around 100, wage is right skewed distribution.

#2.
#plot(density(nls_80$wage[nls_80$married==0]))
#plot(density(nls_80$wage[nls_80$married==1]))
ggplot(nls_80, aes(x = wage, colour = factor(married))) +
  geom_density()

#3,

wage_south <- nls_80$wage[nls_80$south==1]
wage_urban <- nls_80$wage[nls_80$urban==1]
wage_sb <- data.frame(cbind(wage_south,wage_urban))
report <- sapply(wage_sb,FUN = function(x) c(Mean = mean(x), 
                                             SD = sd(x), 
                                             Min = min(x), 
                                             Max = max(x)))
report

#4
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("sjlabelled")
library(sjPlot)
library(sjmisc)
library(sjlabelled)

reg_1 <- lm(wage ~ educ,data = nls_80)
summary(reg_1)
tab_model(reg_1)
#problem of this reg_1,the intercept of p-value > 0.05 so the intercept is
#not signficant,and the r-square is too low as 0.1,so i decided to drop intercept
#and make reg_2 which has no intercept for the regression.
reg_2 <- lm(wage ~ 0+educ,data = nls_80)
summary(reg_2)
tab_model(reg_2)
#estimator beta is 70.84,and the p-value < 0.05 , r-squared value is 0.864 which
#is much bigger than the results of reg_1, which means this linear model is more
#fit the true data and could explain more about variance in wage.

#5,
reg_3 <- lm(wage ~ 0+educ+iq,data = nls_80)
summary(reg_3)
tab_model(reg_3)
#estimator beta is 38.25 of wage on educ，beta of iq on wage is 4.39.

#6
#between reg_2 and reg_3, the coefficients of wage on educ is different, when we 
#add one more variable like iq, which iq could have correlation with educ,
#and we know it from the scatter plot matrix.it cause the estimator is smaller 
#than the first simple linear regression just between educ and wage.
#and it may cause multicollinearity and affects the coefficients and we
# may not trust the p-values.
library(dplyr)
library(car)
vif(reg_3)
#the result of vif is high which means we should choose one of iq and educ as 
#independent variables.
