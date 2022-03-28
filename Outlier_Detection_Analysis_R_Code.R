##Setting the working directory and importing the train and test dataset
setwd("C:\\Users\\jonah.muniz\\OneDrive - Accenture\\Masters Program\\Unsupervised Learning\\Assignment_4_Muniz")
dataset_train <- read.csv("C:\\Users\\jonah.muniz\\OneDrive - Accenture\\Masters Program\\Unsupervised Learning\\Assignment_4_Muniz\\assignment-4-option-1-training.csv",head=TRUE,sep=",")
dataset_test <- read.csv("C:\\Users\\jonah.muniz\\OneDrive - Accenture\\Masters Program\\Unsupervised Learning\\Assignment_4_Muniz\\assignment-4-option-1-test.csv",head=TRUE,sep=",")

#Improt necessary libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(pROC)
library(caret)
library(reshape2)
library(dbscan)
library(fpc)
library(solitude)
#EDA
head(dataset_train)
head(dataset_test)
summary(dataset_train)
summary(dataset_test)

#Remove rows with NAs and duplicates
dataset_train_na_rm <- na.omit(dataset_train)
dataset_train_dedup <- dataset_train_na_rm[!duplicated(dataset_train_na_rm),]
summary(dataset_train_dedup)

dataset_test_na_rm <- na.omit(dataset_test)
dataset_test_dedup <- dataset_test_na_rm[!duplicated(dataset_test_na_rm),]
summary(dataset_test_dedup)

#EDA Cont.
normalize <- function(x) {
  denom <- ifelse(max(x) - min(x) == 0, 1, max(x) - min(x))
  return ((x - min(x)) / denom)
}
dataset_train_num_norm <- normalize(dataset_train_dedup[,(3:4)])
dataset_test_num_norm <- normalize(dataset_test_dedup[,(3:4)])

ggplot(dataset_train_dedup, aes(x = Quant, y = Val)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "Quantity", y = "Value") +
  labs(alpha = "", colour="Legend")
#Convert Prod to numeric and add to train set
prod_num<-as.factor(dataset_train_dedup$Prod)
prod_num <- unclass(prod_num)
dataset_train_dedup$Prod_num <- prod_num
dataset_train_num_norm$Prod_num <- prod_num

prod_num_test<-as.factor(dataset_test_dedup$Prod)
prod_num_test <- unclass(prod_num_test)
dataset_test_dedup$Prod_num <- prod_num_test
dataset_test_num_norm$Prod_num <- prod_num_test

#Perform iforest
iso = isolationForest$new(num_trees = 500)
iso$fit(dataset_train_num_norm)

scores_train =  dataset_train_num_norm %>%
  iso$predict() %>%
  arrange(desc(anomaly_score))

summary(scores_train)
#Add prediction scores to train_dataset
dataset_train_num_norm$anomaly_score <- scores_train$anomaly_score
dataset_train_num_norm$average_depth <- scores_train$average_depth
#Label Outlier or Normal
dataset_train_num_norm$Outlier <- as.factor(ifelse(dataset_train_num_norm$anomaly_score >= 0.5887,"1","0"))

ggplot(dataset_train_num_norm, aes(x = Prod_num, y = anomaly_score, color = Outlier)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "Product Number", y = "Anomaly Score") +
  labs(alpha = "", colour="Legend")

#Fit to test data
scores_test = dataset_test_num_norm %>%
  iso$predict() %>%
  arrange(desc(anomaly_score))

summary(scores_test)

ggplot(scores_test, aes(x = id, y = average_depth)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "id", y = "average_depth")

#Add prediction scores to test dataset
dataset_test_num_norm$anomaly_score <- scores_test$anomaly_score
dataset_test_num_norm$average_depth <- scores_test$average_depth

#Label Outlier or Normal
dataset_test_num_norm$Insp_Predict <- as.factor(ifelse(dataset_test_num_norm$anomaly_score  >= 0.5887,"1","0"))
dataset_test_num_norm$Insp_Actual <- as.factor(ifelse(dataset_test_dedup$Insp == "ok","0","1"))
#Confusion Matrix
confmat <- confusionMatrix(data=dataset_test_num_norm$Insp_Predict, reference = dataset_test_num_norm$Insp_Actual)
confmat
#Precision, Recall, F1
precision = 10166/(10166+1714)
recall = 10166/(10166+1153)
fscore = (2*precision*recall)/(precision+recall)
fscore

#Convert production number to numeric
dataset_train_num_norm$Prod_num <- as.numeric(dataset_train_num_norm[,3])
dataset_test_num_norm$Prod_num <- as.numeric(dataset_test_num_norm[,3])

#Perform lof Train
lofresults_train <- lof(dataset_train_num_norm[,c(1,2,3)], minPts = 3)
lofresults_train_new <- ifelse(lofresults_train == "Inf", 100, lofresults_train)
dataset_train_num_norm$lof_score <- lofresults_train_new
dataset_train_num_norm$Insp_Predict_lof <- as.factor(ifelse(lofresults_train_new > 2, "1","0"))

ggplot(dataset_train_num_norm, aes(x = Prod_num, y = lof_score, color = Insp_Predict_lof)) + 
  geom_point(shape = 1, alpha = 0.5) +
  labs(x = "Product Number", y = "lof Score") +
  labs(alpha = "", colour="Legend")

#Perform lof Test
lofresults <- lof(dataset_test_num_norm[,c(1,2,3)], minPts = 3)
lofresults_new <- ifelse(lofresults == "Inf", 100, lofresults)
summary(lofresults_new)
boxplot(lofresults_new)
dataset_test_num_norm$Insp_Predict_lof <- as.factor(ifelse(lofresults_new > 2, "1","0"))
#Confusion Matrix
confmat_lof <- confusionMatrix(data=dataset_test_num_norm$Insp_Predict_lof, reference = dataset_test_num_norm$Insp_Actual)
confmat_lof

#Precision, Recall, F1
precision_lof = 10229/(10229+1651)
recall_lof = 10229/(10229+479)
fscore_lof = (2*precision_lof*recall_lof)/(precision_lof+recall_lof)
fscore_lof
