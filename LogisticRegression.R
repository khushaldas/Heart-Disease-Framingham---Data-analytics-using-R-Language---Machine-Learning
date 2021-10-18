#Khushal Das
#F2020313013

install.packages("ggplot2") #It is graphics framework used for ploting data 
install.packages("dplyr") #It is package used for data manipulation
install.packages("ROCR") #ROCR #visualize and evalute performance of classifier
install.packages("caret") #Caret #KFOLD cross validation
#install.packages('e1071', dependencies=TRUE) #e1071 

library(dplyr) #loading dplyr
library(ggplot2) #loading ggplot2
library(ROCR)#loading ROCR
library(caret) #loaing caret 
#library(e1071) #loading e1071


setwd("E:\\Acedimics\\MS\\Sems_1\\Bussiness_Analytics\\Term_paper")
 
data_ = read.csv('framingham.csv')

head(data_)

colSums(is.na(data_))

names(data_)

data_$education #catagorical Data 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_Edu = getmode(data_$education)

data_$education[is.na(data_$education)] = mode_Edu

mean_Cig_ = mean(data_$cigsPerDay,na.rm=TRUE)
mean_Cig_

data_$cigsPerDay[is.na(data_$cigsPerDay)] = mean_Cig_

data_$BPMeds[is.na(data_$BPMeds)] = 0 #zero means not on blood pressure medications, 96 percent are zero values in dataset so replacing it with zero

mean_totChol_ = mean(data_$totChol,na.rm=TRUE)
mean_totChol_
data_$totChol[is.na(data_$totChol)] = mean_totChol_

data_$male[is.na(data_$male)]=1

mean_BMI = mean(data_$BMI,na.rm=TRUE)
mean_BMI
data_$BMI[is.na(data_$BMI)] = mean_BMI

mean_heartRate = mean(data_$heartRate,na.rm=TRUE)
mean_heartRate
data_$heartRate[is.na(data_$heartRate)] = mean_heartRate

mean_glucose = mean(data_$glucose,na.rm=TRUE)
mean_glucose
data_$glucose[is.na(data_$glucose)] = mean_glucose

colSums(is.na(data_))

summary(data_)

nrow(data_)
ncol(data_)

## 80% of the sample size
smp_size <- floor(0.8 * nrow(data_))

## set the seed to make your partition reproducible
set.seed(123)
train_ <- sample(seq_len(nrow(data_)), size = smp_size)

train <- data_[train_, ]
test <- data_[-train_, ]

nrow(train)

nrow(test)

ncol(test)


logistic_model <- glm(TenYearCHD ~ ., data = train, family = "binomial")

summary(logistic_model)


predictTrain = predict(logistic_model, type="response")

summary(predictTrain)

tapply(predictTrain, train$TenYearCHD, mean)

# Confusion matrix for threshold of 0.5

table(train$TenYearCHD, predictTrain > 0.5)

#sensitivity 
54/644 #0.0838

#speificity
3576/3596 #0.9944383

# Confusion matrix for threshold of 0.7
table(data_$TenYearCHD, predictTrain > 0.7)

#sensitivity 
9/644 #0.01397

#specificity 
3595/3596 #0.9997219


ROCRpred = prediction(predictTrain, train$TenYearCHD)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#Prediction on testing data
predictTest = predict(logistic_model, type = "response", newdata = test)

summary(predictTest)

#Threshold 0.5
table(test$TenYearCHD, predictTest > 0.5)
#Accuracy
(94+90)/258
#0.8691038
#total 737 right prediction from total 848 values

#Threshold 0.7
table(test$TenYearCHD, predictTest > 0.7)
#Accuracy
(729+1)/848
#0.86084

#Threshold 0.3
table(test$TenYearCHD, predictTest > 0.3)
#Accuracy
(658+36)/848
#0.8183962

#Threshold 0.8
table(test$TenYearCHD, predictTest > 0.8)
#Accuracy
(729+1)/848
#0.8608491

#Best Accuracy is at threshold 0.5 which is 0.8691038


#Leave one out cross validation - LOOCV

train$TenYearCHD=as.factor(train$TenYearCHD)


# Define training control
train.control <- trainControl(method = "LOOCV")

# Train the model
model <- train(TenYearCHD ~., data = data_, method = "glm",
               trControl = train.control)

# Summarize the results
summary(model)


#Prediction on testing data
predictTest = predict(model, newdata = test)

summary(predictTest)

#Threshold 0.5
table(test$TenYearCHD, predictTest > 0.5)
#Accuracy
(728+4)/848
# 0.8632075
# K-fold cross-validation

train.control <- trainControl(method = "cv", number = 100)

data_$TenYearCHD=as.factor(data_$TenYearCHD)

model_cv <- train(TenYearCHD ~., data = data_, method = "glm",
                  trControl = train.control)

summary(model_cv)


print(model_cv)

typeof(model_cv)

toString(model_cv)

#Repeated K-fold cross-validation

train.control <- trainControl(method = "repeatedcv", number = 100, repeats = 5)

data_$TenYearCHD=as.factor(data_$TenYearCHD)

model_cv <- train(TenYearCHD ~., data = data_, method = "glm",
               trControl = train.control)

summary(model_cv)


print(model_cv)

typeof(model_cv)

toString(model_cv)



