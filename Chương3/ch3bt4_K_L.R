library(sandwich)
library(car)
## Loading required package: carData
library(lmtest)
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
library(carData)
library(ggplot2)
#UOC LUONG HAM SAN XUAT Y=f(K,L)
# setwd("D:/dataR/ch123")
CH3BT4_K_L <- read.delim("E:/KTL/KTL_01/Chương3/CH3BT4_K_L.TXT")
View(CH3BT4_K_L)
Y=CH3BT4_K_L$Y
K=CH3BT4_K_L$K
L=CH3BT4_K_L$L


#UOC LUONG HAM COBB-DOUGLAS
# Y output, L- lao dong, K- von
# Hoi quy Y = b0 K^b1 L^2b2

reg1=lm(log(Y)~log(K)+log(L))
summary(reg1)
## 
## Call:
## lm(formula = log(Y) ~ log(K) + log(L))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.56455 -0.21716  0.03205  0.16651  0.42318 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  9.77025    0.22857  42.745  < 2e-16 ***
## log(K)       0.52370    0.09376   5.586 3.28e-05 ***
## log(L)       0.69301    0.14054   4.931 0.000127 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.282 on 17 degrees of freedom
## Multiple R-squared:  0.7814, Adjusted R-squared:  0.7557 
## F-statistic: 30.39 on 2 and 17 DF,  p-value: 2.436e-06
# Tinh/hien thị cac gia tri Yhat
LYhat1=fitted(reg1)
LYhat1
# Tinh/hien thi phan dư
e1=resid(reg1)
e1
# Hien thi cac betahat
beta=coef(reg1)
beta
## (Intercept)      log(K)      log(L) 
##   9.7702509   0.5236994   0.6930054
beta[1]
## (Intercept) 
##    9.770251
anova(reg1)
## Analysis of Variance Table
## 
## Response: log(Y)
##           Df Sum Sq Mean Sq F value    Pr(>F)    
## log(K)     1 2.9002 2.90017  36.461  1.33e-05 ***
## log(L)     1 1.9341 1.93408  24.315 0.0001266 ***
## Residuals 17 1.3522 0.07954                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Hien gia tri RSS
deviance(reg1)
## [1] 1.352226
covbetahat=vcov(reg1)
covbetahat
##             (Intercept)       log(K)       log(L)
## (Intercept)  0.05224348 -0.012182545 -0.023211362
## log(K)      -0.01218254  0.008790028 -0.001147648
## log(L)      -0.02321136 -0.001147648  0.019751448
plot(log(Y), type="o", col="blue", pch="o", lty=1, ylim=c(0,max(log(Y))))
lines(LYhat1, col="dark red", lty=3)
points(e1, col="dark red",pch="+")

# legend(0,10,legend=c("LY","LYhat1","e1"), col=c("blue","red","black"),                           pch=c("o","*","+"),lty=c(1,2,3), ncol=1)

plot(log(Y), type="o", col="blue", pch="o", lty=1, ylim=c(8,max(log(Y))))
lines(LYhat1, col="dark red", lty=3)

# Kiem dinh gia thiet
# Kiem dinh b1=0
linearHypothesis(reg1,c("log(K)"))
## Linear hypothesis test
## 
## Hypothesis:
## log(K) = 0
## 
## Model 1: restricted model
## Model 2: log(Y) ~ log(K) + log(L)
## 
##   Res.Df    RSS Df Sum of Sq      F   Pr(>F)    
## 1     18 3.8341                                 
## 2     17 1.3522  1    2.4818 31.201 3.28e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Kiem dinh b1=0.6
linearHypothesis(reg1,c("log(K)=0.6"))
## Linear hypothesis test
## 
## Hypothesis:
## log(K) = 0.6
## 
## Model 1: restricted model
## Model 2: log(Y) ~ log(K) + log(L)
## 
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1     18 1.4049                           
## 2     17 1.3522  1  0.052682 0.6623  0.427

# Kiem dinh b1=b2=0
linearHypothesis(reg1,c("log(K)", "log(L)"))
## Linear hypothesis test
## 
## Hypothesis:
## log(K) = 0
## log(L) = 0
## 
## Model 1: restricted model
## Model 2: log(Y) ~ log(K) + log(L)
## 
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1     19 6.1865                                  
## 2     17 1.3522  2    4.8343 30.388 2.436e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# Kiem dinh b1=b2
# linearHypothesis(reg1,c("log(K)=log(L)"))
linearHypothesis(reg1,c("log(K) - log(L)=0"),test="F")
## Linear hypothesis test
## 
## Hypothesis:
## log(K) - log(L) = 0
## 
## Model 1: restricted model
## Model 2: log(Y) ~ log(K) + log(L)
## 
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1     18 1.4262                           
## 2     17 1.3522  1   0.07394 0.9296 0.3485


# Kiem dinh gia thiet b1+b2=1
lht(reg1,c("log(K) + log(L)=1"),test="F")
## Linear hypothesis test
## 
## Hypothesis:
## log(K)  + log(L) = 1
## 
## Model 1: restricted model
## Model 2: log(Y) ~ log(K) + log(L)
## 
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1     18 1.4946                           
## 2     17 1.3522  1   0.14232 1.7893 0.1986 

##ln(Y)=alpha* +beta[1]ln(K)+(1-beta[1])ln(L)+u
##     =alpha* +beta[1]ln(K/L)+ln(L)+u
## ln(Y/L)=alpha* + beta[1]ln(K/L)+u
## Y/L:năng suất lao động
##K/L:mức trang bị kĩ thuật cho lao động



# Kiem dinh gia thiet b1+b2=1, b2=0.3
# Myh0=c(log(K) + log(L)=1,log(L)=0.3))
lht(reg1,c("log(K) + log(L)=1","log(L)=0.3"),test="F")
## Linear hypothesis test
## 
## Hypothesis:
## log(K)  + log(L) = 1
## log(L) = 0.3
## 
## Model 1: restricted model
## Model 2: log(Y) ~ log(K) + log(L)
## 
##   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
## 1     19 2.1890                              
## 2     17 1.3522  2   0.83676 5.2598 0.01667 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Khoang tin 95% cho cac he so
confint(reg1, level=0.95)
##                 2.5 %     97.5 %
## (Intercept) 9.2880139 10.2524879
## log(K)      0.3258933  0.7215055
## log(L)      0.3964923  0.9895186
# khoang tin cay cho he so cua log(K)
confint(reg1,"log(K)",level=0.90)
##              5 %      95 %
## log(K) 0.3606023 0.6867965
# khoang tin cay cho b1+b2
seb1b2= sqrt(covbetahat[2,2]+covbetahat[3,3]+2*covbetahat[2,3])
seb1b2
## [1] 0.1620067
beta[2]+beta[3]+c(-1,1)*seb1b2
## [1] 1.054698 1.378712


# Du bao
# newdata=data.frame(data)
K=c(50,45)
L=c(35,32)
newdata=cbind(K,L)
newdata
##       K  L
## [1,] 50 35
## [2,] 45 32
newdata=data.frame(newdata)
# Dự bao diem
prereg1=predict(reg1,newdata)
prereg1
##        1        2 
## 14.28285 14.16557
# Du bao khoang
prereg2=predict(reg1,newdata,interval = 'confidence', level=0.95)
prereg2
##        fit      lwr      upr
## 1 14.28285 13.48183 15.08387
## 2 14.16557 13.39632 14.93482
