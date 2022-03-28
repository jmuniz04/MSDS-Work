setwd("C:/Users/jonah.muniz/OneDrive - Accenture/Masters Program/Unsupervised Learning/Assignment_3_Muniz")

library(farff) # for reading arff file
library(cvTools) # explicit creation of folds for cross-validation
library(ModelMetrics) # used for precision-recall evaluation of classifiers
library(car) # for recode function

# optimal cutoff for predicting bad credit set as
# (cost of false negative/cost of false positive) times
# (prevalence of positive/prevalence of negative)
# (1/5)*(.3/.7) = 0.086
CUTOFF = 0.086
COSTMATRIX = matrix(c(0,5,1,0), nrow = 2, ncol = 2, byrow = TRUE)

credit = readARFF("dataset_31_credit-g.arff")
# write to comma-delimited text for review in Excel
write.csv(credit, file = "credit.csv", row.names = FALSE)

# check structure of the data frame
cat("\n\nStucture of initial credit data frame:\n")
print(str(credit))

# quick summary of credit data
cat("\n\nSummary of initial credit data frame:\n")
print(summary(credit))

# personal_status has level "female single" with no observations
cat("\n\nProblems with personal_status, no single females:\n")
print(table(credit$personal_status))
# fix this prior to analysis
credit$personal_status = factor(as.numeric(credit$personal_status),
    levels = c(1,2,3,4), 
    labels = c("male div/sep","female div/dep/mar","male single","male mar/wid"))

cat("\n\nProblems with purpose, low- and no-frequency levels:\n")
print(table(credit$purpose))
# keep first four classes: "new car", "used car", "furniture/equipment", "radio/tv"
# keep "education" and "business" with new values 
# add "retraining" to "education"
# gather all other levels into "other"
credit$purpose = recode(credit$purpose, '"new car" = "new car";
    "used car" = "used car"; 
    "furniture/equipment" = "furniture/equipment";
    "radio/tv" = "radio/tv"; 
    "education" = "education"; "retraining" = "education";
    "business" = "business"; 
    "domestic appliance" = "other"; "repairs" = "other"; "vacation" = "other"; 
    "other" = "other" ',
    levels = c("new car","used car","furniture/equipment","radio/tv", 
    "education","business","other" ))

# credit_amount is highly skewed... use log_credit_amount instead
credit$log_credit_amount = log(credit$credit_amount)    

# summary of transformed credit data
cat("\n\nSummary of revised credit data frame:\n")
print(summary(credit))

# logistic regression evaluated with cross-validation
# include explanatory variables except foreign_worker
# (only 37 of 100 cases are foreign workers)
credit_model = "class ~ checking_status + duration + 
    credit_history + purpose + log_credit_amount + savings_status + 
    employment + installment_commitment + personal_status +        
    other_parties + residence_since + property_magnitude +
    age + other_payment_plans + housing + existing_credits +      
    job + num_dependents + own_telephone" 

set.seed(1)
nfolds = 5
folds = cvFolds(nrow(credit), K = nfolds) # creates list of indices

baseprecision = rep(0, nfolds)  # precision with 0 cutoff
baserecall = rep(0, nfolds)  # recall with  0 cutoff
basef1Score = rep(0, nfolds)  # f1Score with 0 cutoff
basecost = rep(0, nfolds)  # total cost with 0 cutoff
ruleprecision = rep(0, nfolds)  # precision with CUTOFF rule
rulerecall = rep(0, nfolds)  # recall with CUTOFF rule
rulef1Score = rep(0, nfolds)  # f1Score with CUTOFF rule
rulecost = rep(0, nfolds)  # total cost with CUTOFF rule

for (ifold in seq(nfolds)) {
    # cat("\n\nSUMMARY FOR IFOLD:", ifold) # checking in development
    # print(summary(credit[(folds$which == ifold),]))
    # train model on all folds except ifold
    train = credit[(folds$which != ifold), ]
    test = credit[(folds$which == ifold),]
    credit_fit = glm(credit_model, family = binomial,
        data = train)
    # evaluate on fold ifold    
    credit_predict = predict.glm(credit_fit, 
        newdata = test, type = "response") 
    baseprecision[ifold] = ppv(as.numeric(test$class)-1, 
        credit_predict, cutoff = 0.5)  
    baserecall[ifold] = recall(as.numeric(test$class)-1, 
        credit_predict, cutoff = 0.5) 
    basef1Score[ifold] = f1Score(as.numeric(test$class)-1, 
        credit_predict, cutoff = 0.5) 
    basecost[ifold] = sum(
        confusionMatrix(as.numeric(test$class)-1,
        credit_predict) * COSTMATRIX)  
    ruleprecision[ifold] = ppv(as.numeric(test$class)-1, 
        credit_predict, cutoff = CUTOFF)  
    rulerecall[ifold] = recall(as.numeric(test$class)-1, 
        credit_predict, cutoff = CUTOFF) 
    rulef1Score[ifold] = f1Score(as.numeric(test$class)-1, 
        credit_predict, cutoff = CUTOFF)
    rulecost[ifold] = sum(
        confusionMatrix(as.numeric(test$class)-1, 
            credit_predict,cutoff=CUTOFF) * COSTMATRIX)                                    
} 
cvbaseline = data.frame(baseprecision, baserecall, basef1Score, basecost,
    ruleprecision, rulerecall, rulef1Score, rulecost)

cat("\n\nCross-validation summary across folds:\n")
print(round(cvbaseline, digits = 3))

cat("\n\nCross-validation baseline results under cost cutoff rules:")
cat("\n    F1 Score: ", round(mean(cvbaseline$rulef1Score), digits = 3))
cat("\n    Average cost per fold: ", 
    round(mean(cvbaseline$rulecost), digits = 2), "\n")

# prepare data for input to autoencoder work
design_matrix = model.matrix(as.formula(credit_model), data = credit)
design_data_frame = as.data.frame(design_matrix)[,-1]  # dropping the intercept term

# normalize the data
minmaxnorm <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
minmax_data_frame <- as.data.frame(lapply(design_data_frame, FUN = minmaxnorm)) 

cat("\n\nStructure of minmax_data_frame for input to autoencoding work:\n")
print(str(minmax_data_frame))




#Encoder Decoder and Autoencoder Section
library(keras)
input_size = ncol(train_auto)
latent_size = 15
#Defining the encoder
enc_input = layer_input(shape = input_size)
enc_output = enc_input %>% 
    layer_dense(units=30, activation = "tanh") %>% 
    layer_dense(units = 20, activation = "tanh") %>%
    layer_dense(units=latent_size) 

encoder = keras_model(enc_input, enc_output)
summary(encoder)

#Defining the decoder
dec_input = layer_input(shape = latent_size)
dec_output = dec_input %>% 
    layer_dense(units=20, activation = "tanh") %>%
    layer_dense(units=30, activation = "tanh") %>%
    layer_dense(units = input_size)

decoder = keras_model(dec_input, dec_output)

summary(decoder)

#Defining the autoencoder
aen_input = layer_input(shape = input_size)
aen_output = aen_input %>% 
    encoder() %>% 
    decoder()

aen = keras_model(aen_input, aen_output)
summary(aen)

aen %>% compile(optimizer="adam", loss="mse",metrics = c('accuracy'))
aen %>% fit(train_auto,train_auto, epochs=100, batch_size=32,validation_data=list(test_auto,test_auto))

aen.mse <- evaluate(aen, train_auto,train_auto)
aen.mse
#Saving the encoder of the autoencoder to be used as the model to produce the new input data for regression model
encoder %>% save_model_tf("encoder")
#Loading the encoder model
new_encoder <- load_model_tf("encoder")
#Creating new train and test dataset to train and validate new log regression model
x_train_encode <- as.data.frame(predict(new_encoder, train_auto))
x_test_encode <- as.data.frame(predict(new_encoder, test_auto))
train_class <- credit[train_ind, ]
test_class <- credit[-train_ind, ]

x_train_encode$class <- train_class$class
x_test_encode$class <- test_class$class
encode_dataframe <- rbind(x_train_encode,x_test_encode)
encode_model = "class ~ V1 + V2 + 
    V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 +
    V13 + V14 + V15"
set.seed(3)
nfolds = 5
folds = cvFolds(nrow(encode_dataframe), K = nfolds) # creates list of indices

encodebaseprecision = rep(0, nfolds)  # precision with 0 cutoff
encodebaserecall = rep(0, nfolds)  # recall with  0 cutoff
encodebasef1Score = rep(0, nfolds)  # f1Score with 0 cutoff
encodebasecost = rep(0, nfolds)  # total cost with 0 cutoff
encoderuleprecision = rep(0, nfolds)  # precision with CUTOFF rule
encoderulerecall = rep(0, nfolds)  # recall with CUTOFF rule
encoderulef1Score = rep(0, nfolds)  # f1Score with CUTOFF rule
encoderulecost = rep(0, nfolds)  # total cost with CUTOFF rule

for (ifold in seq(nfolds)) {
    # cat("\n\nSUMMARY FOR IFOLD:", ifold) # checking in development
    #print(summary(credit[(folds$which == ifold),]))
    # train model on all folds except ifold
    train = encode_dataframe[(folds$which != ifold), ]
    test = encode_dataframe[(folds$which == ifold),]
    encode_fit = glm(encode_model, family = binomial,
                   data = train)
    # evaluate on fold ifold    
    encode_predict = predict.glm(encode_fit, 
                               newdata = test, type = "response") 
    encodebaseprecision[ifold] = ppv(as.numeric(test$class)-1, 
                                     encode_predict, cutoff = 0.5)  
    encodebaserecall[ifold] = recall(as.numeric(test$class)-1, 
                                     encode_predict, cutoff = 0.5) 
    encodebasef1Score[ifold] = f1Score(as.numeric(test$class)-1, 
                                       encode_predict, cutoff = 0.5) 
    encodebasecost[ifold] = sum(
        confusionMatrix(as.numeric(test$class)-1,
                        encode_predict) * COSTMATRIX)  
    encoderuleprecision[ifold] = ppv(as.numeric(test$class)-1, 
                                     encode_predict, cutoff = CUTOFF)  
    encoderulerecall[ifold] = recall(as.numeric(test$class)-1, 
                                     encode_predict, cutoff = CUTOFF) 
    encoderulef1Score[ifold] = f1Score(as.numeric(test$class)-1, 
                                       encode_predict, cutoff = CUTOFF)
    encoderulecost[ifold] = sum(
        confusionMatrix(as.numeric(test$class)-1, 
                        encode_predict,cutoff=CUTOFF) * COSTMATRIX)                                    
}

encodebaseline = data.frame(encodebaseprecision, encodebaserecall, encodebasef1Score, encodebasecost,
                            encoderuleprecision, encoderulerecall, encoderulef1Score, encoderulecost)

cat("\n\nCross-validation summary across folds:\n")
print(round(encodebaseline, digits = 3))

cat("\n\nCross-validation baseline results under cost cutoff rules:")
cat("\n    F1 Score: ", round(mean(encodebaseline$encoderulef1Score), digits = 3))
cat("\n    Average cost per fold: ", 
    round(mean(encodebaseline$encoderulecost), digits = 2), "\n")
