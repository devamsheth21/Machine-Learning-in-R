data = read.csv("Admission.csv")


summary(data)

k=10
acc= rep(NA,k)

for (i in 1:k)
  
{
  
  library(caTools)
  
  split = sample.split(data$admit , SplitRatio = 0.7)
  
  train = subset(data, split=="TRUE")
  test =  subset(data, split=="FALSE")
  
  
  model = glm(admit~. ,data=train, family="binomial")
  summary(model)
  
  prediction = predict(model, test , type="response")
  
  final_pred = ifelse(prediction>=0.5,1,0)
  
  table(Actual = test$admit, Predicted = final_pred)
  
  cnfmatrix = as.matrix(table(Actual = test$admit, Predicted = final_pred))
  
  diagterms = diag(cnfmatrix)
  totalterms = sum(cnfmatrix)
  
  Accuracy = sum(diagterms)/totalterms
  acc[i]=Accuracy
  
}

mean(acc)