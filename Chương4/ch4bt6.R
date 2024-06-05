#BAI TAP 4.6

library(lmtest)
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
library(ggplot2)
# Thu nhap(LWAGE) phụ thuộc vào kinh nghiệm(GRADE),công đoàn(UNION)

ch4bt6_m <- read.delim("E:/KTL/KTL_01/ch4bt6_m.txt")
ch4bt6_m
summary(ch4bt6_m)
##LWAGE            GRADE           UNION      
##Min.   :-3.579   Min.   : 3.00   Min.   :0.000  
##1st Qu.: 1.351   1st Qu.:11.00   1st Qu.:0.000  
##Median : 1.671   Median :12.00   Median :0.000  
##Mean   : 1.649   Mean   :11.77   Mean   :0.244  
##3rd Qu.: 1.991   3rd Qu.:12.00   3rd Qu.:0.000  
##Max.   : 4.052   Max.   :16.00   Max.   :1.000 

# wage= thu nhap; grade=nam kinh nghiem; trade = 1 
#neu tham gia cong doan,=0 neu khong tham gia  

#GÁN BIẾN
lwage=ch4bt6_m$LWAGE
grade=ch4bt6_m$GRADE
union=ch4bt6_m$UNION

#XÉT MÔ HÌNH HỒI QUY
reg=lm(lwage ~ grade+union)
summary(reg)

## Call:
## lm(formula = lwage ~ grade + union)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.2792 -0.2615  0.0254  0.3188  2.5061 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.69679    0.05279   13.20   <2e-16 ***
## grade        0.07718    0.00442   17.46   <2e-16 ***
## union        0.18119    0.01797   10.08   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5096 on 4357 degrees of freedom
## Multiple R-squared:  0.08492,    Adjusted R-squared:  0.0845 
## F-statistic: 202.2 on 2 and 4357 DF,  p-value: < 2.2e-16

#ẢNH HUONG TUONG TAC GRADE vs UNION
# XÉT MÔ HÌNH
reg1=lm(lwage~grade+union+grade*union)
summary(reg1)
## Call:
## lm(formula = lwage ~ grade + union + grade * union)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -5.2782 -0.2621  0.0254  0.3178  2.5055 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.706175   0.056594  12.478   <2e-16 ***
## grade       0.076380   0.004748  16.088   <2e-16 ***
## union       0.110690   0.154082   0.718    0.473    
## grade:union 0.005999   0.013022   0.461    0.645    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5097 on 4356 degrees of freedom
## Multiple R-squared:  0.08497,    Adjusted R-squared:  0.08434 
## F-statistic: 134.8 on 3 and 4356 DF,  p-value: < 2.2e-16
