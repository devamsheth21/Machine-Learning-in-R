data=iris
#data= read.csv("wisc_bc_data.csv")
featdata=data[,-5]
#featdata=featdata[,-1]
model= kmeans(featdata, 3)
model

tab=table(data$Species,model$cluster)
accu = (sum(diag(tab)))/sum(tab)
accu
plot(x=data$Sepal.Length,y=data$Petal.Width,col=model$cluster)
model$iter
