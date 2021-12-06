data= read.csv("wisc_bc_data.csv")
d=data[,(-1:-2)]
l=length(d)
for(i in 1:l)
{
  d[i]=(d[i]-min(d[i]))/(max(d[i])-min(d[i]))
}
diagnosis=data$diagnosis
d=cbind(diagnosis,d)
library(caTools)
library(class)

nn=20
acc=rep(NA,nn)
for(j in 1:nn)
{
  split=sample.split(d$diagnosis,SplitRatio = 0.7)
  train=subset(d,split=="TRUE")
  test=subset(d,split=="FALSE")
  training=train[-1]
  testing=test[-1]
  train_class=train$diagnosis
  test_class=test$diagnosis
  pred = knn(training,testing,cl=train_class,k=j)
  tab = table(actual =test_class, predicted = pred)
  tab
  
  accuracy = sum(diag(tab)/sum(tab))*100
  acc[j]=accuracy
  
}
acc
mean(acc)
max(acc)

plot(acc,t="l")
