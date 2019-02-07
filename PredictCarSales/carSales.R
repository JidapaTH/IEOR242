library(dplyr)
library(ggplot2)
library(car)
library(GGally)

setwd("C:/Users/hp/Documents/R/242")
wl <- read.csv("Wrangler242-Spring2019.csv")
str(wl)

ggscatmat(wl, columns = 1:8, alpha = 0.8)

#### 1)

# split data
wl.train <- filter(wl, Year <= 2015) 
head(wl.train)
tail(wl.train)
wl.test <- filter(wl, Year > 2015)
head(wl.test)

# train model
mod1 <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.All + CPI.Energy, data = wl.train)
# Check the correlation
vif(mod1)
# result
# Unemployment WranglerQueries         CPI.All      CPI.Energy 
#    68.852495        4.509533       69.700881        8.410345 
# CPI.All has the highest VFI score so drop it

# train new model
mod2 <- lm(WranglerSales ~ Unemployment + WranglerQueries + CPI.Energy, data = wl.train)
summary(mod2)
# Check the correlation
vif(mod2)
# Unemployment WranglerQueries      CPI.Energy 
#     4.450958        4.353009        1.048267 

# Though all scores are below 5, only 1 variables is significant
# Unemployment has the highest VFI score so drop it
# train model
mod3 <- lm(WranglerSales ~ WranglerQueries , data = wl.train)
summary(mod3)
# Check the correlation
vif(mod3)
# WranglerQueries      CPI.Energy 
#        1.016816        1.016816 
# All VIF scores are low, so the model is acceptable

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -4463.50    2772.88  -1.610    0.112    
# WranglerQueries   262.34      16.14  16.259   <2e-16 ***
# CPI.Energy         14.95      10.92   1.369    0.175    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1743 on 69 degrees of freedom
# Multiple R-squared:  0.7933,	Adjusted R-squared:  0.7873 
# F-statistic: 132.4 on 2 and 69 DF,  p-value: < 2.2e-16

# still the only significant variables is WranglerQueries

# Add month factors
mod2.1 <- lm(WranglerSales ~ MonthFactor + Unemployment + WranglerQueries + CPI.All + CPI.Energy, data = wl.train)
vif(mod2.1)

mod2.2 <- lm(WranglerSales ~ MonthFactor + Unemployment + WranglerQueries +  CPI.Energy, data = wl.train)
vif(mod2.2)
summary(mod2.2)
# Call:
#   lm(formula = WranglerSales ~ MonthFactor + Unemployment + WranglerQueries + 
#        CPI.Energy, data = wl.train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3061.5  -688.7   -84.5   537.7  7992.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            256.834   6559.735   0.039 0.968905    
# MonthFactorAugust     -187.864    942.863  -0.199 0.842777    
# MonthFactorDecember    256.402   1031.202   0.249 0.804530    
# MonthFactorFebruary   -983.912    902.526  -1.090 0.280223    
# MonthFactorJanuary   -3099.912    934.576  -3.317 0.001588 ** 
# MonthFactorJuly       -460.458    994.666  -0.463 0.645179    
# MonthFactorJune        -91.368    958.238  -0.095 0.924371    
# MonthFactorMarch      -159.170    883.455  -0.180 0.857660    
# MonthFactorMay        1726.566    907.616   1.902 0.062187 .  
# MonthFactorNovember  -1397.907    956.182  -1.462 0.149242    
# MonthFactorOctober    -511.285    961.533  -0.532 0.596973    
# MonthFactorSeptember  -954.041    883.330  -1.080 0.284669    
# Unemployment          -247.251    460.022  -0.537 0.593031    
# WranglerQueries        220.469     58.655   3.759 0.000404 ***
# CPI.Energy              14.079      9.789   1.438 0.155853    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1528 on 57 degrees of freedom
# Multiple R-squared:  0.8687,	Adjusted R-squared:  0.8365 
# F-statistic: 26.94 on 14 and 57 DF,  p-value: < 2.2e-16

mod3 <- lm(WranglerSales ~ MonthFactor + WranglerQueries, data = wl.train)
vif(mod3)
summary(mod3)
wlPredictions <- predict(mod3, newdata=wl.test)
SSE = sum((wl.test$WranglerSales - wlPredictions)^2)
SST = sum((wl.test$WranglerSales - mean(wl.train$WranglerSales))^2)
OSR2 = 1 - SSE/SST
OSR2 # 0.6728674

# predict Jan 2019
# data from www.goodcarbadcar.net and https://trends.google.com/trends/explore
wlPredictions <- predict(mod3, newdata=data.frame(MonthFactor='January',WranglerQueries=71))
wlPredictions

