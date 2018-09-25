install.packages("randomForrest")
library(Metrics)
library(caret)
library(randomForest)
library(car)
library(corrplot)
library(knitr)
library(dplyr)
library(ggplot2)

path <- "C:\\Users\\CHIRAG\\Downloads\\ACADgILd\\project\\project4"
setwd(path)
cancer_data<- read.csv("CancerData.csv")

#Exploratory Analysis

summary(cancer_data)

cancer_data<- select(cancer_data,-c(id,X))

dim(cancer_data)

kable(head(cancer_data))

#Outlier detection
boxplot(cancer_data$area_mean)

#row number with the max vakue in area_mean
which(cancer_data$area_mean == max(cancer_data$area_mean))

#remove that row
cancer_data=cancer_data[!cancer_data$area_mean == max(cancer_data$area_mean),]

#repeat
boxplot(cancer_data$area_se)
which(cancer_data$area_se==max(cancer_data$area_se))
cancer_data=cancer_data[!cancer_data$area_se == max(cancer_data$area_se),]

#for outlier detection for loop boxplot
par(mfrow=c(3,3))
for (i in 2:31){
  boxplot(cancer_data[,i])
}


#Missing value treatment 
#checking for missing values
for (i in 1:31){
  print(sum(is.na(cancer_data[,i])))
}
#No missing value in this data set 

#checking correlation 
corrplot(cor(cancer_data[,-1]))
findCorrelation(x=cor(cancer_data[,-1]), cutoff=0.9)

#Spliting the data set 
train <- sample(x= 1:nrow(cancer_data),size = 0.8*nrow(cancer_data),replace = FALSE)

train_df<- cancer_data[train,]
test_df<- cancaer_data[-train,]
dim(train_df)
dim(test_df)

#logistic model
model1<- glm(diagnosis~.,family="binomial",data = train_df)
summary(model1)
# AIC = 62

#Cross Validation
tr_cntrl = trainControl(method="cv", number = 5)
model2 = train(diagnosis~., train_df,method="glm",trControl=tr_cntrl)
summary(model2)

#Random forrest 

model3= randomForest(diagnosis~., train_df)
summary(model3)

#Prediction

pred1 = predict(model1,test_df,type = 'response')
pred2 = predict(model2,test_df)
pred3 = predict(model3,test_df)

confusionMatrix(data = pred2,reference = test_df$diagnosis)
confusionMatrix(data = pred3,reference = test_df$diagnosis)

#choosing the best variables from the data
varImp(model1)
varImp(model3)   # select top 5 with highest value from this 

