#Same sample size


library(ggplot2)
data('diamonds')
df=diamonds

set.seed(40)
rand = sample(1:nrow(df),500)
train = df[rand,]
set.seed(78)
rand = sample(1:nrow(df),500)
test = df[rand,]
train_test_err=data.frame(matrix(ncol = 3,nrow = 0))

complexity=list(1,2,3,4,5,6,7,8,9)

#-------------MODEL 1----------------------

m1=lm(price~carat,train)
m1
tr=sum(m1$residuals^2)
pred = predict(m1, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(1, tr, te))

#-------------MODEL 2----------------------

m2 <- lm(price~carat + I(carat^2), train)

tr=sum(m2$residuals^2)
pred = predict(m2, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(2, tr, te))

#-------------MODEL 3----------------------

m3 <- lm(price ~ carat + I(carat^2) + I(carat^3), train)

tr=sum(m3$residuals^2)
pred = predict(m3, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(3, tr, te))

#-------------MODEL 4---------------------

m4 <- lm(price ~ carat + I(carat^2) + I(carat^3) +
           I(carat^4), train)

tr=sum(m4$residuals^2)
pred = predict(m4, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(4, tr, te))

#-------------MODEL 5----------------------

m5 <- lm(price ~ carat + I(carat^2) + I(carat^3) 
         + I(carat^4) + I(carat^5), train)

tr=sum(m5$residuals^2)                  
pred = predict(m5, newdata=test)
te=sum((pred-test$price)^2)               
train_test_err <- rbind(train_test_err, c(5, tr, te))


#-------------MODEL 6----------------------

m6 <- lm(price ~ carat + I(carat^2) + I(carat^3) 
         + I(carat^4) + I(carat^5)+ I(carat^5) 
         + I(carat^6), train)

tr=sum(m6$residuals^2)
pred = predict(m6, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(6, tr, te))

#-------------MODEL 7----------------------

m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) 
         + I(carat^4) + I(carat^5)+ I(carat^5) 
         + I(carat^6) + I(carat^6) + I(carat^7), train)

tr=sum(m7$residuals^2)
pred = predict(m7, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(7, tr, te))

#-------------MODEL 8----------------------

m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) 
         + I(carat^4) + I(carat^5)+ I(carat^5) 
         + I(carat^6) + I(carat^6) + I(carat^7)
         + I(carat^8), train)

tr=sum(m8$residuals^2)
pred = predict(m8, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(8, tr, te))

#-------------MODEL 9----------------------

m9 <- lm(price ~ carat + I(carat^2) + I(carat^3) 
         + I(carat^4) +
           I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8) + I(carat^9) , train)

tr=sum(m9$residuals^2)
pred = predict(m9, newdata=test)
te=sum((pred-test$price)^2)
train_test_err <- rbind(train_test_err, c(9, tr, te))


colnames(train_test_err) <- c('Model Complexity', 'Train_Error',
                              'Test_Error')



plot(train_test_err$`Model Complexity`,train_test_err$Test_Error,
     pch=19,cex=0.5,type='l',lwd=2,xlab = 'Model Complexity',
     ylab = 'Test Error',col='blue')
plot(train_test_err$`Model Complexity`,train_test_err$Train_Error,
      pch=19,type='l',lwd=2,xlab = 'Model Complexity', 
      ylab = 'Train Error',col='red')


#----------------------------------------------------------

par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(train_test_err$`Model Complexity`,train_test_err$Test_Error, type="l",ylab = 'Test Error',xlab='Model Complexity',col='blue')
par(new = T)
plot(train_test_err$`Model Complexity`,train_test_err$Train_Error, type='l' ,axes=FALSE,, xlab = "", ylab = "",col='red')
axis(side = 4)
mtext(side = 4, line = 3, 'Train Error')     
legend(6.5, 1.6e9, legend=c("Test Error", "Train Error"),
       col=c('blue','red'), lty=1:1, cex=0.63)
       