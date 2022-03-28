# Define path and file;
my.path <- 'C:\\Users\\jonah.muniz\\OneDrive - Accenture\\Masters Program\\Capstone\\';
my.file <- paste(my.path,'credit_card_default.RData',sep='');

# Read the RData object using readRDS();
credit_card_default <- readRDS(my.file)

# Show dataframe structure;
str(credit_card_default)

# Here is the observation count in each data set;
table(credit_card_default$data.group)

# Show top of data frame with some values;
head(credit_card_default)

#Data Quality Checks
library(dlookr)
summary(credit_card_default)
diagnose(credit_card_default)
diagnose_numeric(credit_card_default)
#Subset rows where Education = 0
credit_card_default[credit_card_default$MARRIAGE == 0,] 
#Replace Marriage = 0 to 3 = Other
credit_card_default$MARRIAGE <- replace(credit_card_default$MARRIAGE, credit_card_default$MARRIAGE == 0,3) 
#Replace Education = 0 to 4 = Other
credit_card_default$EDUCATION <- replace(credit_card_default$EDUCATION, credit_card_default$EDUCATION == 0,4) 
#Rows where Education is greater than 4
Education_Data_Quality_Issues <- credit_card_default[credit_card_default$EDUCATION > 4,]
#Replace values gretaer than 4 within EDUCATION to 4
credit_card_default$EDUCATION <- replace(credit_card_default$EDUCATION, credit_card_default$EDUCATION == 5,4) 
credit_card_default$EDUCATION <- replace(credit_card_default$EDUCATION, credit_card_default$EDUCATION == 6,4) 
#Analyze Repayment Status
Repay_0_Data_Quality_Issues <- credit_card_default[credit_card_default$PAY_0 < -1,]
Repay_0_Data_Quality_Issues
#Replace -2 with 2
credit_card_default$PAY_0 <- replace(credit_card_default$PAY_0, credit_card_default$PAY_0 == -2,-1)
credit_card_default$PAY_2 <- replace(credit_card_default$PAY_2, credit_card_default$PAY_2 == -2,-1)
credit_card_default$PAY_3 <- replace(credit_card_default$PAY_3, credit_card_default$PAY_3 == -2,-1)
credit_card_default$PAY_4 <- replace(credit_card_default$PAY_4, credit_card_default$PAY_4 == -2,-1)
credit_card_default$PAY_5 <- replace(credit_card_default$PAY_5, credit_card_default$PAY_5 == -2,-1)
credit_card_default$PAY_6 <- replace(credit_card_default$PAY_6, credit_card_default$PAY_6 == -2,-1)
#Pay_0 to Pay_1
credit_card_default$PAY_1 <- credit_card_default$PAY_0
#Analyze Utilization Issues
Utilization_Data_Quality_Issues <- credit_card_default[credit_card_default$Utilization_1 > 1,]
#-------------------------------------------------------------------------------------------------------------------
#Feature Engineering

#Age Binning
credit_card_default$AGE_18_25 <- 
  ifelse(credit_card_default$AGE >= 18 & credit_card_default$AGE <= 25, 1, 0)
credit_card_default$AGE_26_40 <- 
  ifelse(credit_card_default$AGE >= 26 & credit_card_default$AGE <= 40, 1, 0)
credit_card_default$AGE_41_100 <- 
  ifelse(credit_card_default$AGE >= 41 & credit_card_default$AGE <= 100, 1, 0)
#Average Bill Amount
credit_card_default$Avg_Bill_Amt <- rowMeans(credit_card_default[ , c(13:18)], na.rm=TRUE)
#Average Payment Amount
credit_card_default$Avg_Pay_Amt <- rowMeans(credit_card_default[ , c(19:24)], na.rm=TRUE)
#Utilization
credit_card_default$Utilization_1 <- credit_card_default$BILL_AMT1/credit_card_default$LIMIT_BAL
credit_card_default$Utilization_2 <- credit_card_default$BILL_AMT2/credit_card_default$LIMIT_BAL
credit_card_default$Utilization_3 <- credit_card_default$BILL_AMT3/credit_card_default$LIMIT_BAL
credit_card_default$Utilization_4 <- credit_card_default$BILL_AMT4/credit_card_default$LIMIT_BAL
credit_card_default$Utilization_5 <- credit_card_default$BILL_AMT5/credit_card_default$LIMIT_BAL
credit_card_default$Utilization_6 <- credit_card_default$BILL_AMT6/credit_card_default$LIMIT_BAL
#Utilization_Max_1
credit_card_default$Utilization_1_Max_1 <- replace(credit_card_default$Utilization_1, credit_card_default$Utilization_1 > 1,1)
credit_card_default$Utilization_2_Max_1 <- replace(credit_card_default$Utilization_2, credit_card_default$Utilization_2 > 1,1)
credit_card_default$Utilization_3_Max_1 <- replace(credit_card_default$Utilization_3, credit_card_default$Utilization_3 > 1,1)
credit_card_default$Utilization_4_Max_1 <- replace(credit_card_default$Utilization_4, credit_card_default$Utilization_4 > 1,1)
credit_card_default$Utilization_5_Max_1 <- replace(credit_card_default$Utilization_5, credit_card_default$Utilization_5 > 1,1)
credit_card_default$Utilization_6_Max_1 <- replace(credit_card_default$Utilization_6, credit_card_default$Utilization_6 > 1,1)
#Average Utilization
credit_card_default$Avg_Utilization <- rowMeans(credit_card_default[ , c(37,39:43)], na.rm=TRUE)
#Average Utilization Max 1
credit_card_default$Avg_Utilization_Max_1 <- rowMeans(credit_card_default[ , c(38,44:48)], na.rm=TRUE)
#6M Balance Growth
credit_card_default$Six_Month_Bal_Grwth <- credit_card_default$BILL_AMT6 - credit_card_default$BILL_AMT1
#6M Utilization Growth
credit_card_default$Six_Month_Utilization_Grwth <- credit_card_default$Utilization_6 - credit_card_default$Utilization_1
#6M Utilization Growth Max 1
credit_card_default$Six_Month_Utilization_Grwth_Max_1 <- credit_card_default$Utilization_6_Max_1 - credit_card_default$Utilization_1_Max_1
#Max Bill Amount
credit_card_default$Max_Bill_Amt <- pmax(credit_card_default$BILL_AMT1, credit_card_default$BILL_AMT2,
                                         credit_card_default$BILL_AMT3, credit_card_default$BILL_AMT4,
                                         credit_card_default$BILL_AMT5,credit_card_default$BILL_AMT6)
#Max Payment Amount
credit_card_default$Max_Pay_Amt <- pmax(credit_card_default$PAY_AMT1, credit_card_default$PAY_AMT2,
                                        credit_card_default$PAY_AMT3, credit_card_default$PAY_AMT4,
                                        credit_card_default$PAY_AMT5,credit_card_default$PAY_AMT6)
#Payment Ratio Bill2 <-> Pay1
credit_card_default$Pay_Ratio_1 <- ifelse(credit_card_default$PAY_AMT1/credit_card_default$BILL_AMT2 == 'NaN', 100, credit_card_default$PAY_AMT1/credit_card_default$BILL_AMT2)
credit_card_default$Pay_Ratio_2 <- ifelse(credit_card_default$PAY_AMT2/credit_card_default$BILL_AMT3 == 'NaN', 100, credit_card_default$PAY_AMT2/credit_card_default$BILL_AMT3)
credit_card_default$Pay_Ratio_3 <- ifelse(credit_card_default$PAY_AMT3/credit_card_default$BILL_AMT4 == 'NaN', 100, credit_card_default$PAY_AMT3/credit_card_default$BILL_AMT4)
credit_card_default$Pay_Ratio_4 <- ifelse(credit_card_default$PAY_AMT4/credit_card_default$BILL_AMT5 == 'NaN', 100, credit_card_default$PAY_AMT4/credit_card_default$BILL_AMT5)
credit_card_default$Pay_Ratio_5 <- ifelse(credit_card_default$PAY_AMT5/credit_card_default$BILL_AMT6 == 'NaN', 100, credit_card_default$PAY_AMT5/credit_card_default$BILL_AMT6)
#Average Payment Ratio
credit_card_default$Avg_Payment_Ratio <- rowMeans(credit_card_default[ , c(56:60)], na.rm=TRUE)
credit_card_default$Avg_Payment_Ratio <- replace(credit_card_default$Avg_Payment_Ratio, credit_card_default$Avg_Payment_Ratio == 'Inf',100)
#Max Delinquency
credit_card_default$Max_Deliquency <- pmax(credit_card_default$PAY_0, credit_card_default$PAY_2,
                                           credit_card_default$PAY_3, credit_card_default$PAY_4,
                                           credit_card_default$PAY_5,credit_card_default$PAY_6)
credit_card_default$Max_Deliquency <- replace(credit_card_default$Max_Deliquency, credit_card_default$Max_Deliquency <0,0)
#Max Utilization
credit_card_default$Max_Utilization <- pmax(credit_card_default$Utilization_1, credit_card_default$Utilization_2,
                                            credit_card_default$Utilization_3, credit_card_default$Utilization_4,
                                            credit_card_default$Utilization_5,credit_card_default$Utilization_6)
#Max Utilization Max 1
credit_card_default$Max_Utilization_Max_1 <- pmax(credit_card_default$Utilization_1_Max_1,credit_card_default$Utilization_2_Max_1,
                                                  credit_card_default$Utilization_3_Max_1,credit_card_default$Utilization_4_Max_1,
                                                  credit_card_default$Utilization_5_Max_1,credit_card_default$Utilization_6_Max_1)
#Pay Amount Binning (Income Bins)
credit_card_default$Low_Income <- 
  ifelse(credit_card_default$Max_Pay_Amt >= 0 & credit_card_default$Max_Pay_Amt <= 2198, 1, 0)
credit_card_default$Medium_Income <- 
  ifelse(credit_card_default$Max_Pay_Amt >= 2199 & credit_card_default$Max_Pay_Amt <= 12100, 1, 0)
credit_card_default$High_Income <- 
  ifelse(credit_card_default$Max_Pay_Amt >= 12101 & credit_card_default$Max_Pay_Amt <= 1684260, 1, 0)
#-------------------------------------------------------------------------------------------------------------------
#EDA
library(stargazer)
#Summary Statistic Table of Engineered Features
#Dataset containing only Engineerd Features
credit_card_default_eng_feat <- credit_card_default[,c("AGE_18_25","AGE_26_40","AGE_41_100",
                                                       "Avg_Bill_Amt","Avg_Pay_Amt","Avg_Utilization",
                                                       "Six_Month_Bal_Grwth","Six_Month_Utilization_Grwth",
                                                       "Max_Bill_Amt","Max_Pay_Amt","Avg_Payment_Ratio",
                                                       "Max_Deliquency","Max_Utilization","Low_Income",
                                                       "Medium_Income","High_Income","DEFAULT",
                                                       "train","test","validate")]
credit_card_default_eng_feat_v2 <- credit_card_default[,c("AGE_18_25","AGE_26_40","AGE_41_100",
                                                          "Avg_Bill_Amt","Avg_Pay_Amt","Avg_Utilization_Max_1",
                                                          "Six_Month_Bal_Grwth","Six_Month_Utilization_Grwth_Max_1",
                                                          "Max_Bill_Amt","Max_Pay_Amt","Avg_Payment_Ratio",
                                                          "Max_Deliquency","Max_Utilization_Max_1","Low_Income",
                                                          "Medium_Income","High_Income","DEFAULT",
                                                          "train","test","validate")]
file_name_sum <- 'Engineered_Feautres_Summary_Table.html'
stargazer(credit_card_default_eng_feat,type=c('html'), out=paste(my.path,file_name_sum,sep=''),
          title=c('Table 1: Summary Statistics for Engineered Features'),
          align = TRUE,digits=2,digits.extra=2, intial.zero=TRUE,median=TRUE)
#Histograms of Important Features
hist(credit_card_default$LIMIT_BAL, main="Figure 1: Histogram of Customer Credit Limit",xlab="Credit Limit",breaks=50, 
     xlim = range(credit_card_default$LIMIT_BAL))
hist(credit_card_default$Avg_Bill_Amt, main="Figure 10: Histogram of Customer Average Bill Amount",xlab="Average Bill Amount",breaks=50, 
     xlim = range(-1000,500000))
hist(credit_card_default$Avg_Pay_Amt, main="Figure 11: Histogram of Customer Average Pay Amount",xlab="Average Pay Amount",
     breaks=100,xlim = range(0,100000))
hist(credit_card_default$Avg_Utilization, main="Figure 12: Histogram of Customer Average Utilization",xlab="Average Utilization",
     breaks=100,xlim = range(-0.1,1.5))
hist(credit_card_default$Six_Month_Bal_Grwth, main="Figure 3: Histogram of Customer 6-Month Balance Growth",
     xlab="6-Month Balance Growth",breaks = 50, xlim = range(-600000,400000))
hist(credit_card_default$Six_Month_Utilization_Grwth, main="Figure 13: Histogram of Customer 6-Month Utilization Growth",
     xlab="6-Month Utilization Growth",breaks = 50, xlim = range(-3,3))
hist(credit_card_default$Max_Bill_Amt, main="Figure 14: Histogram of Customer Maximum Bill Amount",xlab="Maximum Bill Amount",
     breaks = 500, xlim = range(credit_card_default$Max_Bill_Amt))
hist(credit_card_default$Max_Pay_Amt, main="Figure 2: Histogram of Customer Maximum Payment Amount",xlab="Maximum Payment Amount",
     breaks = 500, xlim = range(0,500000))
#Boxplot
boxplot(credit_card_default$BILL_AMT1,credit_card_default$BILL_AMT2,credit_card_default$BILL_AMT3,
        credit_card_default$BILL_AMT4,credit_card_default$BILL_AMT5,credit_card_default$BILL_AMT6,
        names=c("Bill Amount 1","Bill Amount 2","Bill Amount 3","Bill Amount 4","Bill Amount 5","Bill Amount 6"))

boxplot(credit_card_default$PAY_AMT1,credit_card_default$PAY_AMT2,credit_card_default$PAY_AMT3,
        credit_card_default$PAY_AMT4,credit_card_default$PAY_AMT5,credit_card_default$PAY_AMT6,
        names=c("Pay Amount 1","Pay Amount 2","Pay Amount 3","Pay Amount 4","Pay Amount 5","Pay Amount 6"))

boxplot(credit_card_default$Utilization_1,credit_card_default$Utilization_2,credit_card_default$Utilization_3,
        credit_card_default$Utilization_4,credit_card_default$Utilization_5,credit_card_default$Utilization_6,
        names=c("Utilization 1","Utilization 2","Utilization 3","Utilization 4","Utilization 5","Utilization 6"))
#Correlation Analysis
feature_correlation_matrix <- cor(credit_card_default)
file_name_cor <- "Credit_Default_Correlation_Matrix"
stargazer(feature_correlation_matrix, title="Table 2: Credit Card Default Correlation Matrix",
          type=c('html'),out=paste(my.path,file_name_cor,sep=''))
#Model Based EDA
library(rpart)				        
library(rattle)					
library(rpart.plot)				
library(RColorBrewer)				
library(party)				
library(partykit)				
library(caret)
#Model EDA Feat Eng v1
form <- as.formula(DEFAULT ~ .)
tree.3 <- rpart(form,credit_card_default_eng_feat)
prp(tree.3)
fancyRpartPlot(tree.3, main="Figure 4: Dendrogram of Feautred Engineered Credit Card Default Dataset")

#Model EDA Feat Eng v2
form <- as.formula(DEFAULT ~ .)
tree.4 <- rpart(form,credit_card_default_eng_feat_v2)
prp(tree.4)
fancyRpartPlot(tree.4, main="Figure 4: Dendogram of Feautred Engineered Credit Card Default Dataset")

boxplot(credit_card_default_eng_feat$Max_Deliquency ~ credit_card_default$DEFAULT,
        main="Figure 5: Boxplot of Max_Deliquency by DEFAULT")

boxplot(credit_card_default_eng_feat$Avg_Utilization ~ credit_card_default$DEFAULT,
        main="Figure 6: Boxplot of Avg_Utilization by DEFAULT")
#_______________________________________________________________________________________________________________________________
#Random Forest Modeling
#Split dataset into train, test, validate
train_data <- subset(credit_card_default_eng_feat, train == 1)
train_data$DEFAULT <- factor(train_data$DEFAULT)
test_data <- subset(credit_card_default_eng_feat, test == 1)
test_data$DEFAULT <- factor(test_data$DEFAULT)
validate_data <- subset(credit_card_default_eng_feat, validate == 1)
validate_data$DEFAULT <- factor(validate_data$DEFAULT)
#Load necessary packages
library(randomForest)
require(caTools)
library(pROC)
#RF Model
random_forest_model <- randomForest(
  DEFAULT ~ .,
  data=train_data
)
importance(random_forest_model)
print(random_forest_model)
#Prediction - Train Data
library(caret)
rf_pred_train <- predict(random_forest_model, train_data)
confusionMatrix(train_data$DEFAULT, rf_pred_train)
#ROC - Train
rf_roc_train <- roc(as.numeric(train_data$DEFAULT),as.numeric(rf_pred_train))
print(rf_roc_train)

#Prediction - Test Data
rf_pred_test <- predict(random_forest_model, test_data)
confusionMatrix(test_data$DEFAULT, rf_pred_test)
#ROC-Test
rf_roc_test <- roc(as.numeric(test_data$DEFAULT),as.numeric(rf_pred_test))
print(rf_roc_test)
#Error Rate of RF
plot(random_forest_model)
#Tune mtry
rf_tune <- tuneRF(train_data[,-14], train_data[,14],
                  stepFactor = 0.5,
                  plot = TRUE,
                  ntreeTry = 300,
                  trace = TRUE,
                  improve = 0.01)
#No. of nodes for the trees
hist(treesize(random_forest_model),
     main = "No. of Nodes for the Trees")
#Variable Importance
varImpPlot(random_forest_model,
           sort = TRUE,
           n.var = 10,
           main = "Figure 7: Top 10 Variable Importance")
varUsed(random_forest_model)

#_______________________________________________________________________________________________________________________________
#XgBoost Modeling
#Load necessary packages
library(xgboost)
library(gbm)
library(caret)
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
#Train Data Prep
train_data_no_label <- train_data[-c(17)]
train_label <- as.numeric(train_data[,"DEFAULT"])-1
trainm <- sparse.model.matrix(DEFAULT ~., data = train_data)
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
#test data prep
test_data_no_label <- test_data[-c(17)]
test_label <- as.numeric(test_data[,"DEFAULT"])-1
testm <- sparse.model.matrix(DEFAULT~., data = test_data)
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

#Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective"="multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)
#XGBoost Model
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       eta = 0.05)
bst_model
#training & test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter,e$train_mlogloss, col = "blue")
lines(e$iter,e$test_mlogloss, col = "red")
min(e$test_mlogloss)
e[e$test_mlogloss == 0.441138,]
#Feature Importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)
#Prediction & confusion matrix - Train
p_train <- predict(bst_model, newdata = train_matrix)
pred_train <- matrix(p_train, nrow = nc, ncol = length(p_train)/nc) %>%
  t() %>%
  data.frame()%>%
  mutate(label = train_label, max_prob = max.col(.,"last")-1)
table(Actual = pred_train$label, Prediction = pred_train$max_prob)
xgb_roc_train <- roc(pred_train$label, pred_train$max_prob)
print(xgb_roc_train)
#Prediction & confusion matrix - Test
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(.,"last")-1)
head(pred)
table(Actual = pred$label, Prediction = pred$max_prob)
xgb_roc <- roc(pred$label,pred$max_prob)
print(xgb_roc)

#_____________________________________________________________________________________________________________________________________
#Logistic Regression w/ Variable Selection
#Load necessary packages
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(pROC)
library(stargazer)


#Full logistic model
full_log_model <- glm(DEFAULT ~.,data=train_data, family = binomial)
coef(full_log_model)
#Fit a important feature logistic model
import_feat <- train_data[,c("Max_Deliquency","Max_Utilization","Avg_Pay_Amt","Max_Pay_Amt","Max_Bill_Amt",
                             "Six_Month_Bal_Grwth","Avg_Payment_Ratio","Avg_Utilization","Six_Month_Utilization_Grwth",
                            "Avg_Bill_Amt","AGE_41_100","AGE_26_40","AGE_18_25","DEFAULT")]
imp_fet_log_model <- glm(DEFAULT ~ Max_Deliquency + Max_Utilization + Avg_Pay_Amt
                         + Max_Pay_Amt + Max_Bill_Amt + Six_Month_Bal_Grwth
                         + Avg_Payment_Ratio + Avg_Utilization + Six_Month_Utilization_Grwth
                         + Avg_Bill_Amt + AGE_41_100 + AGE_26_40 + AGE_18_25, data = train_data, family = binomial) 
coef(imp_fet_log_model)
#Stepwise model
step.model <- imp_fet_log_model %>% stepAIC(trace = FALSE, direction = "both")
summary(step.model)

#New Model Based on Features significant in Step Model
step.model.v2 <- glm(DEFAULT ~ Max_Deliquency + Max_Utilization + Avg_Pay_Amt
                     + Max_Pay_Amt + Max_Bill_Amt 
                     + Avg_Payment_Ratio + Avg_Utilization 
                     + Avg_Bill_Amt + AGE_41_100, data = train_data, family = binomial)
summary(step.model.v2)

#Predicting Prediction Classes - Train
probabilities_step_train <- predict(step.model.v2, train_data, type = "response")

#Determining the threshold value - Train
observed.classes_train <- train_data$DEFAULT
log_roc_train <- roc(observed.classes_train,probabilities_step_train)
print(log_roc_train)
plot(log_roc_train)
smooth_roc_train <- smooth(roc=log_roc_train)
print(smooth_roc_train)
plot(smooth_roc_train)

roc.specs.train <- coords(roc=log_roc_train,x=c('best'),
                          ret=c('threshold','specificity','sensitivity'))
print(roc.specs.train)

#Predicting Prediction Classes - Train
probabilities_step_train <- predict(step.model.v2, train_data, type = "response")
predicted.classes_step_train <- ifelse(probabilities_step_train>roc.specs.train$threshold,1,0)

#Model Monitoring Plan - KS
# Create a data frame for model.score and response
my.df <- as.data.frame(cbind(probabilities_step_train,train_data$DEFAULT));
head(my.df)

decile.pts <- quantile(my.df$probabilities_step_train,
                       probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));
decile.pts

my.df$model.decile <- cut(my.df$probabilities_step_train,breaks=c(0,decile.pts,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                       '11','12','13','14','15','16','17','18','19','20'))
);

head(my.df)

aggregate(my.df$probabilities_step_train,by=list(Decile=my.df$model.decile),FUN=min);

table(my.df$model.decile)

table(my.df$model.decile,my.df$V2)

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$V2)[,1],
                               Y1=table(my.df$model.decile,my.df$V2)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09','10',
                                            '11','12','13','14','15','16','17','18','19','20'))
));
ks.table[order(ks.table$Decile),]
#Confusion Matrix - Train
observed.classes_train <- train_data$DEFAULT
log_confusion_matrix_train <- confusionMatrix(as.factor(observed.classes_train),as.factor(predicted.classes_step_train))
log_confusion_matrix_train
log_roc_train <- roc(observed.classes_train,probabilities_step_train)
print(log_roc_train)

#Predicting Prediction Classes - Test
probabilities_step_test <- predict(step.model.v2, test_data, type = "response")

#Determining the threshold value - Test
observed.classes_test <- test_data$DEFAULT
log_roc_test <- roc(observed.classes_test,probabilities_step_test)
print(log_roc_test)
plot(log_roc_test)
smooth_roc_test <- smooth(roc=log_roc_test)
print(smooth_roc_test)
plot(smooth_roc_test)

roc.specs.test <- coords(roc=log_roc_test,x=c('best'),
                         ret=c('threshold','specificity','sensitivity'))
print(roc.specs.test)

#Predicting Prediction Classes - Test
probabilities_step_test <- predict(step.model.v2, test_data, type = "response")
predicted.classes_step_test <- ifelse(probabilities_step_test>roc.specs.test$threshold,1,0)

#Determining the threshold value - Train
observed.classes_train <- train_data$DEFAULT
log_roc_train <- roc(observed.classes_train,probabilities_step_train)
print(log_roc_train)
plot(log_roc_train)
smooth_roc_train <- smooth(roc=log_roc_train)
print(smooth_roc_train)
plot(smooth_roc_train)

roc.specs.train <- coords(roc=log_roc_train,x=c('best'),
                          ret=c('threshold','specificity','sensitivity'))
print(roc.specs.train)

#Predicting Prediction Classes - Train
probabilities_step_train <- predict(step.model.v2, train_data, type = "response")
predicted.classes_step_train <- ifelse(probabilities_step_train>roc.specs.train$threshold,1,0)

#Model Monitoring Plan - KS
# Create a data frame for model.score and response
my.df <- as.data.frame(cbind(probabilities_step_test,test_data$DEFAULT));
head(my.df)

decile.pts <- quantile(my.df$probabilities_step_test,
                       probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));
decile.pts

my.df$model.decile <- cut(my.df$probabilities_step_test,breaks=c(0,decile.pts,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                       '11','12','13','14','15','16','17','18','19','20'))
);

head(my.df)

aggregate(my.df$probabilities_step_test,by=list(Decile=my.df$model.decile),FUN=min);

table(my.df$model.decile)

table(my.df$model.decile,my.df$V2)

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$V2)[,1],
                               Y1=table(my.df$model.decile,my.df$V2)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09','10',
                                            '11','12','13','14','15','16','17','18','19','20'))
));
ks.table[order(ks.table$Decile),]

#Confusion Matrix - Test
observed.classes_test <- test_data$DEFAULT
log_confusion_matrix_test <- confusionMatrix(as.factor(observed.classes_test),as.factor(predicted.classes_step_test))
log_confusion_matrix_test
log_roc_test <- roc(observed.classes_test,predicted.classes_step_test)
print(log_roc_test)

#Predicting Prediction Classes - Validate
probabilities_step_validate <- predict(step.model.v2, validate_data, type = "response")
predicted.classes_step_validate <- ifelse(probabilities_step_validate>roc.specs.validate$threshold,1,0)

#Determining the threshold value - Validate
observed.classes_validate <- validate_data$DEFAULT
log_roc_validate <- roc(observed.classes_validate,probabilities_step_validate)
print(log_roc_validate)
plot(log_roc_validate)
smooth_roc_validate <- smooth(roc=log_roc_validate)
print(smooth_roc_validate)
plot(smooth_roc_validate)

roc.specs.validate <- coords(roc=log_roc_validate,x=c('best'),
                          ret=c('threshold','specificity','sensitivity'))
print(roc.specs.validate)



my.df <- as.data.frame(cbind(probabilities_step_validate,validate_data$DEFAULT));
head(my.df)

decile.pts <- quantile(my.df$probabilities_step_validate,
                       probs=c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95));
decile.pts

my.df$model.decile <- cut(my.df$probabilities_step_validate,breaks=c(0,decile.pts,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10',
                                       '11','12','13','14','15','16','17','18','19','20'))
);

head(my.df)

aggregate(my.df$probabilities_step_validate,by=list(Decile=my.df$model.decile),FUN=min);

table(my.df$model.decile)

table(my.df$model.decile,my.df$V2)

ks.table <- as.data.frame(list(Y0=table(my.df$model.decile,my.df$V2)[,1],
                               Y1=table(my.df$model.decile,my.df$V2)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09','10',
                                            '11','12','13','14','15','16','17','18','19','20'))
));
ks.table[order(ks.table$Decile),]

#Confusion Matrix - Validate
observed.classes_validate <- validate_data$DEFAULT
log_confusion_matrix_validate <- confusionMatrix(as.factor(observed.classes_validate),as.factor(predicted.classes_step_validate))
log_confusion_matrix_validate
log_roc_validate <- roc(observed.classes_validate,predicted.classes_step_validate)
print(log_roc_validate)

#Final Summary Table of log Model
log.file.name <- 'log_reg_initial_features.html'
stargazer(import_feat, type = c('html'), out=paste(my.path,log.file.name,sep=''),
          title = c('Table 4: Summary of Important Feature List'), align = TRUE,
          digits = 2, digits.extra=2,initial.zero=TRUE)
log.file.name <- 'log_reg_summary_table.html'
stargazer(step.model.v2, type = c('html'), out=paste(my.path,log.file.name,sep=''),
          title = c('Table 5: Model #3'), align = TRUE,
          digits = 2, digits.extra=2,initial.zero=TRUE)

#___________________________________________________________________________________________________________________________________
#Neural Net Model
#Load necessary packages
library(tidyverse)
library(keras)
library(fastDummies)
library(caret)
library(tensorflow)

#Scale Train and Test Datasets
X_train <- train_data_no_label  %>% 
  scale()

y_train <- to_categorical(train_label)
X_test <- test_data_no_label %>% 
  scale()
y_test <- to_categorical(test_label)

# Network design
model <- keras_model_sequential()
model %>%
  # Input layer
  layer_dense(units = 256, activation = 'relu', input_shape =  ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  # Hidden layer
  layer_dense(units = 75, activation = 'relu') %>%
  # Output layer
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

# Network config
history <- model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Running our data
model %>% fit(
  X_train, y_train, 
  epochs = 20, 
  batch_size = 5
)

summary(model)

# Calculating accuracy
predictions_train <- model %>% predict_classes(X_train)

# Confusion Matrix
observed.classes.nn <- as.integer(train_data$DEFAULT)-1
nn_confusion_matrix <- confusionMatrix(as.factor(observed.classes.nn),as.factor(predictions_train))
nn_confusion_matrix

#ROC/AUC
nn_roc <- roc(observed.classes.nn,predictions_train)
print(nn_roc)
plot(nn_roc)

# Calculating accuracy - Test
predictions_test <- predict_classes(model, X_test)

# Confusion Matrix - Test
observed.classes.nn.test <- as.integer(test_data$DEFAULT)-1
nn_confusion_matrix_test <- confusionMatrix(as.factor(observed.classes.nn.test),as.factor(predictions_test))
nn_confusion_matrix_test

#ROC/AUC - Test
nn_roc <- roc(observed.classes.nn,predictions_train)
print(nn_roc)
plot(nn_roc)

################################################################################
# Naive Bayes
################################################################################
library(naivebayes)

npredictor.df <- train_data[,-17]
nb.2 <- naive_bayes(x=npredictor.df,y=train_data$DEFAULT)

# Look at output;
nb.2

# Predict the class;
predicted.class <- predict(nb.2);
mean(predicted.class==train_data$DEFAULT)
confusionMatrix(train_data$DEFAULT,predicted.class)
roc(train_data$DEFAULT,as.numeric(predicted.class))


npredictor.df <- test_data[,-17]
nb.2 <- naive_bayes(x=npredictor.df,y=test_data$DEFAULT)

# Look at output;
nb.2

# Predict the class;
predicted.class <- predict(nb.2);
confusionMatrix(test_data$DEFAULT,predicted.class)
roc(test_data$DEFAULT,as.numeric(predicted.class))
