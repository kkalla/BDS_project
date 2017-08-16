## Machine learning for analysis public assets

data <- read.csv('Data/tot_ana.csv',stringsAsFactors=FALSE)

library(cluster)
library(factoextra)
data_catgories <- data$category
data <- data[,-c("SPFC2")]
for(i in 5:10){
	data[,i] <- as.factor(data[,i])
}
data[,c(3:4)] <- data.frame(scale(data[,c(3,4)]))
data_std <- data[,-1]

d_mat <- daisy(data_std,metric="gower")
h <- hclust(d_mat,"ward.D2")
plot(h,labels=F)

fviz_nbclust(data_std,kmeans,"silhouette")

## Naive Bayes classifier
# install.packages('e1071')
library(e1071)

# make train and test set
data <- read.csv('Data/tot_libsoc.csv')
n <- nrow(data)
train <- sample(n,n*0.7,replace=FALSE)
train_data <- data[train,]
test_data <- data[-train,]

# make model
nb_model <- naiveBayes(category~.,data=train_data)

# predict 
nb_pred <- predict(nb_model,test_data[,-1])

## logistic regression
pub_glm <- glm(data_category~.,family=binomial(link='probit'),data=train_data)



## Random Forest
library(randomForest)
library(caret)

fit <- randomForest(category~.,data=train_data,importance=TRUE,ntree=10000)
varImpPlot(fit)
pred <- predict(fit,test_data)

confusionMatrix(pred,test_data$data_category)
# Accuracy = 0.7746(95% CI: 0.7268, 0.8175)
# Mcnemar's Test P-value = 0.05425

## Cforest
# install.packages('party')
library(party)

set.seed(200)
fit_ctree <- cforest(data_category~.,data=train_data,
		     controls = cforest_unbiased(ntree=10000,
						 mtry=3))



## MXNetR
# intall.packages('MXNetR')
# It's not available in 3.4.1 version


## Package deepnet
# install.packages('deepnet')


## Package h2o
h2o.init(nthreads=-1)
model_h2o <- h2o.deeplearning(x=train_data[,1],y=colnames(train_data[,-1]),
			      training_frame=train,validation_frame=test)



## SVM

