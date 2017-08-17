library(e1071)
library(randomForest)
library(caret)
library(factoextra)
library(cluster)



## Read data
tot_pub <- read.csv('Data/tot_publicfc_std.csv')
str(tot_pub)
tot_pub_sub <- tot_pub[!is.na(tot_pub$Land_Value_bySize),-1]
png('fviz_nbclust_publicfc.png')
fviz_nbclust(tot_pub_sub,kmeans,'silhouette')
dev.off()
png('fviz_nbclust_publicfc_wss.png')
fviz_nbclust(tot_pub_sub,kmeans,'wss')
dev.off()

## naive bayes
n <- nrow(tot_pub)
train <- sample(n,n*0.7,replace=F)
train_data <- tot_pub[train,]
test_data <- tot_pub[-train,]

nb_model <- naiveBayes(category~.,data=train_data)

nb_pred <- predict(nb_model,test_data[,-1])
confusionMatrix(nb_pred,test_data$category)

fit <- randomForest(category~.,data=train_data,importance=TRUE,ntree=1000)
png('varimpPlot1.png')
varImpPlot(fit)
dev.off()
pred <- predict(fit,test_data)
confusionMatrix(pred,test_data$category)
