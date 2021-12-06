library(MASS)
d=Boston
d1=d
l=length(d)
for(i in 1:l)
{
  d1[i]=(d1[i]-min(d1[i]))/(max(d1[i])-min(d1[i]))
}
library(caTools)
library(neuralnet)
nn=10
MSE=rep(NA,nn)
for(j in 1:nn)
{
  set.seed(123)
  split=sample.split(d1$medv,SplitRatio = 0.7)
  train=subset(d1,split==TRUE)
  test=subset(d1,split==TRUE)
  model = neuralnet(medv~., train ,hidden=j, err.fct = "sse")
  plot(model)
  predict = compute(model, test[,-14])
  result = predict$net.result
  original_result=(test$medv)*(max(d$medv)-min(d$medv)) + min(d$medv)
  predicted_result= result*(max(d$medv)-min(d$medv)) + min(d$medv)
  MSE[j]=sum((original_result-predicted_result)^2)/length(original_result)
}
MSE
mean(MSE)
max(MSE)
