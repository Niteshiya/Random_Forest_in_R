#Read Data
data <- read.csv("CTG.csv")
str(data)
#Convert NSP into factor varriable
data$NSP <- as.factor(data$NSP)
table(data$NSP)

#Data Partition
set.seed(123)
ind <- sample(2,nrow(data),replace=T,prob=c(0.7,0.3))
training <- data[ind==1,]
test <- data[ind==2,]

#Random Forest
library(randomForest)
rf <- randomForest(NSP ~.,data=training)
print(rf)
attributes(rf)
rf$confusion

#Prediction and Confusion matrix
library(caret)
p1 <- predict(rf,training)
table(predicted=p1,training$NSP)
confusionMatrix(p1,training$NSP)

#prediction with test data
p2 <- predict(rf,test)
table(predicted=p2,test$NSP)
confusionMatrix(p2,test$NSP)

#Error rate
plot(rf)

#tune model 
tuneRF(training[,-22],training[,22],
       stepFactor = 0.5,
       plot = T,
       ntreeTry = 300,
       trace=T,
       improve = 0.05)
#We see at 8 OOB error is lowest so mtry=8
rf <- randomForest(NSP ~.,data=training,
                   ntree=300,
                   mtry=8,
                   importance=T,
                   proximity=T)
rf
p1 <- predict(rf,training)
table(predicted=p1,training$NSP)
p2 <- predict(rf,test)
table(predicted=p2,test$NSP)
confusionMatrix(p2,test$NSP)


#no. of nodes for tree
hist(treesize(rf),
     main="no. of nodes for trees")

#Varriable importance
varImpPlot(rf,
           sort=T,
           n.var=10,
           main="Impotant Varriables")
importance(rf)
varUsed(rf)


#Partial Dependence Plot
partialPlot(rf,training,ASTV,"1")
partialPlot(rf,training,ASTV,"3")
partialPlot(rf,training,ASTV,"2")
#Prediction with astv

#Extract single tree(n=1st tree)
getTree(rf,1,labelVar = T)

#Multi dimensional scaling plot
MDSplot(rf,training$NSP)
