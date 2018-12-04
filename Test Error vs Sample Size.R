#library(ggplot2)
#data('diamonds')
df=diamonds
#--------------------------------------------------------------------------------
#plot(x = df$carat, y = df$price)
samplesize=list(500,700,800,850,900,950,1000,1200,1300)
testerror=list()
  
for (i in samplesize){
  set.seed(0)
  rand2 = sample(1:nrow(df),i)
  train = df[rand2,]
  set.seed(5)
  rand3 = sample(1:nrow(df),500)
  test = df[rand3,]
  #View(test)
  
  
  m3 <- lm(price ~ carat + I(carat^2) + I(carat^3), train)
  
  plot(train$carat,train$price, pch=19, cex=0.5)
  lines(sort(train$carat), fitted(m3)[order(train$price)], col='brown', type='l',pch=20) 
  
  pred = predict(m3, newdata=test)
  x=sum((pred-test$carat)^2)
  testerror=append(testerror,x)
}

plot(samplesize,testerror, pch=19, cex=0.5,ylab="Test Error",xlab="Sample Size")
lines(samplesize,testerror, col='red', type='l',pch=20) 


#--------------------------------------------------------------
  
  
