## Regression for different models complexities with same sample size



library(ggplot2)
data('diamonds')
df=diamonds
#--------------------------------------------------------------------------------

set.seed(125)
rand2 = sample(1:nrow(df),500)
train = df[rand2,]

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================

m1 <- lm(price ~ carat, train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================

m2 <- lm(price ~ carat + I(carat^2), train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 3
#=============================================================================================

m3 <- lm(price ~ carat + I(carat^2) + I(carat^3), train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 4
#=============================================================================================

m4 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4), train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 5
#=============================================================================================

m5 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) 
         + I(carat^5), train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 6
#=============================================================================================

m6 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) 
         + I(carat^5)+ I(carat^5) + I(carat^6), train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) 
         + I(carat^5)+ I(carat^5) + I(carat^6) 
         + I(carat^6) + I(carat^7), train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 8
#=============================================================================================

m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) 
         + I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8), train)
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

m9 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) 
         + I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8) + I(carat^9) , train)



#=============================================================================================
# PLotting all graphs
#=============================================================================================

plot(train$carat,train$price, pch=19, cex=0.1,xlab='Carat',ylab='Price')
lines(sort(train$carat), fitted(m1)[order(train$carat)], col=1, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m2)[order(train$carat)], col=2, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m3)[order(train$carat)], col=3, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m4)[order(train$carat)], col=4, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m5)[order(train$carat)], col=5, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m6)[order(train$carat)], col=6, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m7)[order(train$carat)], col=7, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m8)[order(train$carat)], col=8, type='l',pch=19,lwd=1) 
lines(sort(train$carat), fitted(m9)[order(train$carat)], col=9, type='l',pch=19,lwd=1) 
legend(2.5, 10000, legend=c("Order 1", "Order 2", "Order 3", "Order 4", "Order 5",
                            "Order 6", "Order 7", "Order 8", "Order 9"),
       col=c(1,2,3,4,5,6,7,8,9), lty=1:1, cex=0.5)
