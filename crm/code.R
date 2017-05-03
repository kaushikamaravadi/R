install.packages('e1071')
install.packages('rpart')
install.packages('C50')
install.packages('h2o')
library(caret)
library(e1071)
library(rpart)
library(C50)
library(h2o)

# Loading the data and selecting relevant columns #
data = read.csv('crmfinl.csv')
data_use = data[c(1:30,33)]

# Checking the data type of elements in a dataframe #
apply(data_use,2,class)

# Checking for the number of missing values in the dataset #
sum(is.na(data_use))

# Checking the number of unique elements in each column #
sapply(data_use,function (x) length(unique(x)))

# Changing the data type of all the columns to factor #
final_data = data.frame(sapply(data_use,factor))
final_data$absences = as.numeric(final_data$absences)


# Dividing the dataset between training and validation dataset in the ratio 80:20 #
sample_size = floor(0.8*nrow(final_data))
set.seed(81)
samp <- sample(seq_len(nrow(final_data)), size = sample_size)										

train = final_data[samp,]
test = final_data[-samp,]

# TUning and Training the decision tree #
grid_dt <- expand.grid(.model = c("rules","tree"), .trials = c(1,10,15, 20, 25,30, 35,40,50,60,70,80), .winnow = c("TRUE","FALSE"))
dt_tune <- train( train[,-c(31)], train$G3, "C5.0",metric = "Kappa", tuneGrid = grid_dt,trControl = trainControl(method = "cv", number = 10))
dt_tune$bestTune
pred_test = predict(dt_tune,test[c(1:30)])
confusionMatrix(pred_test,test$G3)
confusionMatrix(predict(dt_tune,train[c(1:30)]),train$G3)

# Tuning and training random Forest #
h2o.init(nthreads=-1, max_mem_size = "2G") 
h2o.getConnection()
rf_tune <- h2o.randomForest(x=names(final_data[c(1:30)]),y="G3",training_frame = as.h2o(train),validation_frame = as.h2o(test),mtries =11,nfolds=10,stopping_metric = c("mean_per_class_error"))
h2o.performance(rf_tune,train=T)
h2o.performance(rf_tune,valid=T)

