library(caret)
library(rpart)
library(rpart.plot)
library(caTools)
library(party)

data=read.csv("wisc_bc_data.csv")
wbcd=data[-1]
  
wbcd$diagnosis=as.character(wbcd$diagnosis)
wbcd$diagnosis[wbcd$diagnosis=="M"]=1
wbcd$diagnosis[wbcd$diagnosis=="B"]=0
wbcd$diagnosis=as.numeric(wbcd$diagnosis)

split=sample.split(wbcd$diagnosis,SplitRatio = 0.7)  
training=subset(wbcd,split=="TRUE")
testing=subset(wbcd,split=="FALSE")



tree2=train(diagnosis~.,training,method="rpart")
tree2
rpart.plot(tree2$finalModel)
plot1=ctree(diagnosis~.,data=training)
plot(plot1)

p2=predict(tree2,testing)
p2
tab2=table(Actual=testing$diagnosis,Predicted=p2)
tab2
accu2=sum(diag(tab2))/sum(tab2)
accu2
