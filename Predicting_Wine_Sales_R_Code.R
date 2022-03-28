mydata <- read.csv("C:\\Users\\jonah.muniz\\OneDrive - Accenture\\Masters Program\\Supervised Learning\\Assignment 10\\wine.csv",head=TRUE,sep=",")

#EDA
summary(mydata)
hist(mydata$Cases)
hist(mydata$STARS)
hist(mydata$star_mean)
hist(mydata$Purchase)
hist(mydata$FixedAcidity)
hist(mydata$VolatileAcidity)
hist(mydata$CitricAcid)
hist(mydata$ResidualSugar)
hist(mydata$Chlorides)
hist(mydata$FreeSulfurDioxide)
hist(mydata$AcidIndex)

mydata$star_mean <- ifelse(is.na(mydata$STARS), mean(mydata$STARS, na.rm = TRUE), mydata$STARS)
mydata$resid_sugar <- ifelse(is.na(mydata$ResidualSugar), mean(mydata$ResidualSugar, na.rm = TRUE),mydata$ResidualSugar)
mydata$chlor <- ifelse(is.na(mydata$Chlorides), mean(mydata$Chlorides, na.rm = TRUE), mydata$Chlorides)
mydata$free_sulfur <- ifelse(is.na(mydata$FreeSulfurDioxide), mean(mydata$FreeSulfurDioxide, na.rm = TRUE), mydata$FreeSulfurDioxide)
mydata$total_sulfur <- ifelse(is.na(mydata$TotalSulfurDioxide), mean(mydata$TotalSulfurDioxide, na.rm = TRUE),mydata$TotalSulfurDioxide)
mydata$ph <- ifelse(is.na(mydata$pH), mean(mydata$pH, na.rm = TRUE), mydata$pH)
mydata$sulph <- ifelse(is.na(mydata$Sulphates), mean(mydata$Sulphates, na.rm = TRUE), mydata$Sulphates)
mydata$alc <- ifelse(is.na(mydata$Alcohol), mean(mydata$Alcohol, na.rm = TRUE), mydata$Alcohol)
mydata_na_mean <- subset(mydata, select =c("star_mean","resid_sugar","chlor","free_sulfur","total_sulfur","ph","sulph","alc"))
summary(mydata_na_mean)
mydatanum <- subset(mydata, select=c("Purchase","Cases",
                                      "star_mean","FixedAcidity","VolatileAcidity",
                                      "CitricAcid","Density","LabelAppeal",
                                      "AcidIndex","resid_sugar","chlor","free_sulfur",
                                     "total_sulfur","ph","sulph","alc"))

require(corrplot)
mcor <- cor(mydatanum)
table_corr = data.frame(mcor)
corrplot(mcor, method="shade", shade.col=NA, tl.col="black",tl.cex=0.5)

plot(mydata$LabelAppeal,mydata$star_mean)
plot(mydata$AcidIndex, mydata$star_na_0)
plot(mydata$VolatileAcidity, mydata$star_na_0)
plot(mydata$Cases, mydata$star_na_0)
plot(mydata$Purchase, mydata$star_na_0)
plot(mydata$star_mean, mydata$Purchase)

#Model
model_1 <- lm(STARS ~Purchase+Cases+LabelAppeal+AcidIndex+alc, data = mydata)
summary(model_1)

model_2 <- glm(Purchase ~ star_mean+AcidIndex, family = binomial, data = mydata)
summary(model_2)
model_2_pred <- ifelse(model_2$fitted.values > 0.50 , 1,0)
model_2_pred
library(car)
vif(model_2)
library(cvms)
library(tibble) 
d_binomial <- tibble("target" = mydata$Purchase,
                     "prediction" = model_2_pred)
basic_table <- table(d_binomial)
basic_table
accuracy <- (394+9847)/nrow(mydata)*100
cfm <- as_tibble(basic_table)
cfm
plot_confusion_matrix(cfm, 
                      target_col = "target", 
                      prediction_col = "prediction",
                      counts_col = "n")
exp(model_2$coefficients)
1-0.640537
plot(model_2)

cooksd <- cooks.distance(model_2)
plot(cooksd)
abline(h = 4/nrow(mydata), col="red")
influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(mydata)))])
mydata_scrub <- mydata[-influential,]
model_2_scrub <- glm(Purchase~star_mean+AcidIndex, family = binomial, data = mydata_scrub)
summary(model_2_scrub)
model_2_pred_scrub <- ifelse(model_2_scrub$fitted.values > 0.50,1,0)

d_binomial_1 <- tibble("target" = mydata_scrub$Purchase,
                     "prediction" = model_2_pred_scrub)
basic_table_1 <- table(d_binomial_1)
basic_table_1
accuracy_scrub <- (9656+606)/nrow(mydata_scrub)*100
cfm_1 <- as_tibble(basic_table_1)
cfm_1
plot_confusion_matrix(cfm_1, 
                      target_col = "target", 
                      prediction_col = "prediction",
                      counts_col = "n")
exp(model_2_scrub$coefficients)
plot(model_2_scrub)

#Extra Credit
cases_sold <- ifelse(model_2_pred_scrub == 1, mydata_scrub$Cases,0)
model_3 <- glm(cases_sold ~ star_mean+LabelAppeal+AcidIndex, family = poisson, data = mydata_scrub)
summary(model_3)
exp(model_3$coefficients)
plot(model_3)
cooksd2 <- cooks.distance(model_3)
influential2 <- as.numeric(names(cooksd2)[(cooksd2 > (4/nrow(mydata)))])
mydata_scrub_2 <- mydata[-influential2,]

model_2_scrub_2 <- glm(Purchase~star_mean+AcidIndex, family = binomial, data = mydata_scrub_2)
model_2_pred_scrub_2 <- ifelse(model_2_scrub_2$fitted.values > 0.50,1,0)
cases_sold_2 <- ifelse(model_2_pred_scrub_2 == 1, mydata_scrub_2$Cases,0)
model_4 <- glm(cases_sold_2 ~ star_mean+LabelAppeal+AcidIndex, family = poisson, data = mydata_scrub_2)
summary(model_4)
exp(model_4$coefficients)
plot(model_4)
pred_model_4 <- model_4$fitted.values
actual_model_4 <- mydata$Cases
#MSE
mse_model_3 <-mean(model_3$residuals^2)
mse_model_4 <- mean(model_4$residuals^2)
