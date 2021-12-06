data = read.csv("Admission.csv")

data$gre = (data$gre - mean(data$gre))/sd(data$gre)
data$gpa = (data$gpa - mean(data$gpa))/sd(data$gpa)
data$rank = (data$rank - mean(data$rank))/sd(data$rank)

k=10
acc= rep(NA,k)

for (i in 1:k)
  
{
  
  library(caTools)
  
  split = sample.split(data$admit , SplitRatio = 0.7)
  
  train = subset(data, split=="TRUE")
  test =  subset(data, split=="FALSE")
  
  library(neuralnet)
  
  model = neuralnet(admit~., train ,hidden=i, err.fct = "sse")
  
  plot(model)
  
  predict = compute(model, test[,-1])
  result = predict$net.result
  
  finalres = ifelse(result>=0.5,1,0)
  
  tab = table(actual = test$admit, predicted = finalres)
  tab
  
  accuracy = sum(diag(tab)/sum(tab))
  acc[i]=accuracy
}
acc
mean(acc)

