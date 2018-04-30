# cluster
setwd("C:/Users/Log/Desktop/数据挖掘课程作业/作业三/titanic")
library(mice)
library(cluster)
file_train <- ('./data/train.csv')
file_test  <- ('./data/test.csv')
data_train <- read.csv(file_train, header = T, sep = ',')
data_test  <- read.csv(file_test , header = T, sep = ',')
data <- rbind(data_train,data_test)
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$SibSp <- as.factor(data$SibSp)
data$Parch <- as.factor(data$Parch)
# 缺失值处理
data <- data[-c(1,4,9,11)]
data$Embarked[c(62,830)] <- 'C'
data$Fare[1044] <- median(data[data$Pclass == '3' & data$Embarked == 'S', ]$Fare, na.rm = TRUE)
set.seed(666)
mice_mod <- mice(data[c(2:8)], method='cart')
mice_output <- complete(mice_mod)
data$Age <- mice_output$Age
cluster_data <- data[-c(1)]
train <- cluster_data[1:891,]
test  <- cluster_data[892:1309,]
target <- data$Survived[c(1:891)]

# k-means
km <- kmeans(data.matrix(train),centers = 3,nstart = 10)
fitted(km)
#结果可视化
table(target,km$cluster)
plot(train[c("Age","Fare")], col = km$cluster, pch = as.integer(target))    
points(km$centers[,c("Age","Fare")], col = 1:3, pch = 8, cex=2); 

#Fuzzy Analysis Clustering
fc <- fanny(data.matrix(train),3,metric = 'SqEuclidean')
clusplot(fc)
table(target,fc$clustering)
