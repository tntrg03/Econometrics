#BAI TAP 4.3
#	Tệp ch4bt3_sbt.txt cho số liệu về tổng chi tiêu (CS) 
#và thu nhập sau thuế (Y) tính theo giá 1985 của UK trong thời gian 1974Q1 - 1984Q4.
#a.  Hãy tạo biến giả trong đó quý I là quý cơ sở.	
#b.  Hãy ước lượng hàm tiêu dùng: CS= beta_1 + beta_2Y + u
#c.  Hãy ước lượng hàm tiêu dùng có ảnh hưởng của yếu tố thời vụ
#d.  Những quý nào có ảnh hưởng đến tiêu dùng. Vì sao?
#e.  Có bỏ được quý II ra khỏi mô hình không? Vì sao?
#f.  Hãy ước lượng hàm tiêu dùng có ảnh hưởng tương tác
#của thu nhập và yếu tố thời vụ.


#CÀI THƯ VIỆN
library(ggplot2)
library(lmtest)
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
library(tsm)
library(urca)
library(sandwich)
library(car)
## Loading required package: carData
library(carData)
library(forecast)
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
library(tseries)
library(dummy)
## dummy 0.1.3
## dummyNews()
library(seasonal)

ch4bt3_sbt <- read.delim("E:/KTL/KTL_01/ch4bt3_sbt.txt")
ch4bt3_sbt

#a.  Hãy tạo biến giả trong đó quý I là quý cơ sở.
#CS= a+bY+c1D2+c2D3+c3D4+ u
CS=ch4bt3_sbt$CS
Y=ch4bt3_sbt$Y
#VẼ ĐỒ THỊ BH SU TUONG QUAN
plot(Y,CS,type="p")

#BIEU THI BANG MA TRAN
CS=ts(CS, start=c(1974,1), end=c(1984,4),frequency = 4)
Y=ts(Y, start=c(1974,1), end=c(1984,4),frequency = 4)
SERIES=cbind(CS,Y)
SERIES
#VẼ ĐT 
autoplot(SERIES,xlab="Q-year",ylab="")

dum3=seasonaldummy(SERIES)
dum3
d1=ts(dum3[,1],start=c(1974,1),end=c(1984,4),frequency = 4)
d2=ts(dum3[,2],start=c(1974,1),end=c(1984,4),frequency = 4)
d3=ts(dum3[,3],start=c(1974,1),end=c(1984,4),frequency = 4)
d1
d2
d3

#b.  Hãy ước lượng hàm tiêu dùng: CS= beta_1 + beta_2Y + u
reg1=lm(CS~Y)
summary(reg1)
## Call:
## lm(formula = CS ~ Y)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2861.8 -1015.9  -142.4   666.0  3150.6 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.988e+03  3.397e+03   0.585    0.562    
## Y           8.514e-01  6.302e-02  13.510   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1512 on 42 degrees of freedom
## Multiple R-squared:  0.8129, Adjusted R-squared:  0.8085 
## F-statistic: 182.5 on 1 and 42 DF,  p-value: < 2.2e-16
# KET LUAN:thu nhập sau thuế(Y)có ảnh hưởng đến chi tiêu(CS)

#c.  Hãy ước lượng hàm tiêu dùng có ảnh hưởng của yếu tố thời vụ
reg2=lm(CS~Y+d1+d2+d3)
summary(reg2)
## Call:
## lm(formula = CS ~ Y + d1 + d2 + d3)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2283.2  -555.1   129.7   580.6  1901.8 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.265e+03  2.254e+03   3.224 0.002556 ** 
## Y            7.876e-01  4.079e-02  19.310  < 2e-16 ***
## d1          -3.147e+03  4.162e+02  -7.562 3.67e-09 ***
## d2          -2.578e+03  4.132e+02  -6.241 2.40e-07 ***
## d3          -1.652e+03  4.103e+02  -4.028 0.000252 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 959.5 on 39 degrees of freedom
## Multiple R-squared:   0.93,  Adjusted R-squared:  0.9229 
## F-statistic: 129.6 on 4 and 39 DF,  p-value: < 2.2e-16

#d.  Những quý nào có ảnh hưởng đến tiêu dùng. Vì sao?
#Cả 3 quý đều ảnh hưởng đến tiêu dùng vì p-value<0.05


plot(reg2)


myH0=c("d1","d2","d3")
myH0
linearHypothesis(reg2, myH0)
## Linear hypothesis test
## 
## Hypothesis:
## d1 = 0
## d2 = 0
## d3 = 0
## 
## Model 1: restricted model
## Model 2: CS ~ Y + d1 + d2 + d3
## 
##   Res.Df      RSS Df Sum of Sq      F    Pr(>F)    
## 1     42 95983889                                  
## 2     39 35902165  3  60081724 21.755 1.915e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#e.  Có bỏ được quý II ra khỏi mô hình không? Vì sao?
#f.  Hãy ước lượng hàm tiêu dùng có ảnh hưởng tương tác
#của thu nhập và yếu tố thời vụ.
reg3=lm(CS~Y+d2+Y*d2)
summary(reg3)

##Call:
##  lm(formula = CS ~ Y + d2 + Y * d2)

##Residuals:
##  Min       1Q   Median       3Q      Max 
##-3151.55  -954.48   -87.84   774.21  3038.34 

##Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
##(Intercept)  700.66480 3851.49361   0.182    0.857    
##  Y              0.87969    0.07127  12.342 3.21e-15 ***
##  d2        6466.26756 7540.90860   0.857    0.396    
##Y:d2          -0.13854    0.14058  -0.985    0.330    
##---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1471 on 40 degrees of freedom
#Multiple R-squared:  0.8314,	Adjusted R-squared:  0.8188 
#F-statistic: 65.76 on 3 and 40 DF,  p-value: 1.619e-15