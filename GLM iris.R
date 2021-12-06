data = iris

summary(data)

setosa = subset(data, data$Species=='setosa')
virginica = subset(data, data$Species=='virginica')
merged = rbind(setosa,virginica)
merged$Species = ifelse(merged$Species =="setosa",1,0)

library(caTools)
split= sample.split(merged$Species, SplitRatio=0.7)
training = subset(merged , split=="TRUE")
test = subset(merged , split=="FALSE")

model = glm(Species~. ,data=training, family="binomial")
summary(model)

prediction = predict(model, test , type="response")

final_pred = ifelse(prediction>=0.5,1,0)

table(Actual = test$Species, Predicted = final_pred)

cnfmatrix = as.matrix(table(Actual = test$Species, Predicted = final_pred))
diagterms = diag(cnfmatrix)
totalterms = sum(cnfmatrix)

Accuracy = sum(diagterms)/totalterms

Accuracy
