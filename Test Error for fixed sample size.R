#Diff sample size

library(ggplot2)
data('diamonds')
df=diamonds

samplesize=list(20,45,82)
testerror=list()

complexity=list(1,2,6,7,8)



set.seed(10)
rand2 = sample(1:nrow(df),20)
#rand2 = sample(1:nrow(df),100)
train = df[rand2,]
set.seed(5)
rand3 = sample(1:nrow(df),20)
#rand3 = sample(1:nrow(df),100)
test = df[rand3,]

test_err=data.frame(matrix(ncol = 2,nrow = 0))

m1 <- lm(price ~ carat, train)
pred = predict(m1, newdata=test)
x=sum((pred-test$carat)^2)
test_err <- rbind(test_err, c(1, x))


m2 <- lm(price ~ carat + I(carat^2), train)
pred = predict(m2, newdata=test)
x=sum((pred-test$carat)^2)
test_err <- rbind(test_err, c(2, x))

m6 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6), train)
pred = predict(m6, newdata=test)
x=sum((pred-test$carat)^2)
test_err <- rbind(test_err, c(6, x))


m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6) 
         + I(carat^6) + I(carat^7), train)
pred = predict(m7, newdata=test)
x=sum((pred-test$carat)^2)
test_err <- rbind(test_err, c(7, x))


m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
           I(carat^5) + I(carat^6) + I(carat^7) +
           I(carat^8), train)
pred = predict(m8, newdata=test)
x=sum((pred-test$carat)^2)
test_err <- rbind(test_err, c(8, x))


colnames(test_err) <- c('Model Complexity','Test_Error')


plot(test_err$`Model Complexity`,test_err$Test_Error, pch=19, cex=0.2,ylab = 'Test Error',xlab = 'Model Complexity')
lines(test_err$`Model Complexity`,test_err$Test_Error, col='yellow', type='l',pch=20) 


c=2
for (i in samplesize){
  
  test_err=data.frame(matrix(ncol = 2,nrow = 0))
  set.seed(i)
  rand2 = sample(1:nrow(df),20)
  #rand2 = sample(1:nrow(df),100)
  train = df[rand2,]
  set.seed(5)
  rand3 = sample(1:nrow(df),20)
  #rand3 = sample(1:nrow(df),100)
  test = df[rand3,]
  #View(test)
  
  
  m1 <- lm(price ~ carat, train)
  pred = predict(m1, newdata=test)
  x=sum((pred-test$carat)^2)
  test_err <- rbind(test_err, c(1, x))
  
  
  m2 <- lm(price ~ carat + I(carat^2), train)
  pred = predict(m2, newdata=test)
  x=sum((pred-test$carat)^2)
  test_err <- rbind(test_err, c(2, x))

  m6 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6), train)
  pred = predict(m6, newdata=test)
  x=sum((pred-test$carat)^2)
  test_err <- rbind(test_err, c(6, x))
  
  
  m7 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) + I(carat^5)+ I(carat^5) + I(carat^6) 
           + I(carat^6) + I(carat^7), train)
  pred = predict(m7, newdata=test)
  x=sum((pred-test$carat)^2)
  test_err <- rbind(test_err, c(7, x))
  
  
  m8 <- lm(price ~ carat + I(carat^2) + I(carat^3) + I(carat^4) +
             I(carat^5) + I(carat^6) + I(carat^7) +
             I(carat^8), train)
  pred = predict(m8, newdata=test)
  x=sum((pred-test$carat)^2)
  test_err <- rbind(test_err, c(8, x))
  
  
  colnames(test_err) <- c('Model Complexity','Test_Error')

  lines(test_err$`Model Complexity`,test_err$Test_Error, col=c, type='l',pch=20)
  c<-c+1
}

