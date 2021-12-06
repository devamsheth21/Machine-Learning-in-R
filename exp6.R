library(caTools)  
library(caret)
library(rpart)
library(rpart.plot)
library(party)

data=iris
split<-sample.split(data$Species,SplitRatio = 0.7)  
training<-subset(data,split=="TRUE")
testing<-subset(data,split=="FALSE")
tree2<-train(Species~.,training,method="rpart")
tree2
plot1=ctree(Species~.,data=training)
plot(plot1)
rpart.plot(tree2$finalModel)
p2<-predict(tree2,testing)
p2
tab2<-table(Actual=testing$Species,Predicted=p2)
tab2
accu2<-sum(diag(tab2))/sum(tab2)
accu2