data <- read.csv('students.csv')[c(-1,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-20)]

data$sex <- as.factor(ifelse(data$sex == 'M', 'Male', 'Female'))

data$schoolsup <- factor(data$schoolsup)

data$famsup <- factor(data$famsup)

data$paid <- factor(data$paid)

data$activities <- factor(data$activities)

data$higher <- factor(data$higher)

data$internet <- factor(data$internet)

data$romantic <- factor(data$romantic)

data$famrel <- as.factor(ifelse(data$famrel == 1, 'Very Bad',
                                ifelse(data$famrel == 2, 'Bad',
                                       ifelse(data$famrel == 3, 'Moderate',
                                              ifelse(data$famrel == 4, 'Good','Excellent')))))

data$freetime <- as.factor(ifelse(data$freetime == 1, 'None',
                                ifelse(data$freetime == 2, 'Little',
                                       ifelse(data$freetime == 3, 'Moderate',
                                              ifelse(data$freetime == 4, 'A Lot','Exponential')))))

data$goout <- as.factor(ifelse(data$goout == 1, 'Never',
                                  ifelse(data$goout == 2, 'Ocassionally',
                                         ifelse(data$goout == 3, 'Often',
                                                ifelse(data$goout == 4, 'Frequently','Very Frequently')))))

data$Dalc <- as.factor(ifelse(data$Dalc == 1, 'None to Very Low',
                               ifelse(data$Dalc == 2, 'Low',
                                      ifelse(data$Dalc == 3, 'Moderate',
                                             ifelse(data$Dalc == 4, 'High','Very High')))))

data$Walc <- as.factor(ifelse(data$Walc == 1, 'None to Very Low',
                              ifelse(data$Walc == 2, 'Low',
                                     ifelse(data$Walc == 3, 'Moderate',
                                            ifelse(data$Walc == 4, 'High','Very High')))))

data$health <- as.factor(ifelse(data$health == 1, 'Very Bad',
                                ifelse(data$health == 2, 'Bad',
                                       ifelse(data$health == 3, 'Moderate',
                                              ifelse(data$health == 4, 'Good','Excellent')))))
data <- data
write.csv(data, "student_data.csv", row.names = FALSE)

library(caTools)

set.seed(123)
split = sample.split(data$G3, SplitRatio = 0.8)
train_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)

write.csv(train_set, "student_train.csv", row.names = FALSE)
write.csv(test_set, "student_test.csv", row.names = FALSE)

#Multilinear regression
multi = lm(formula = G3 ~ ., data = train_set)

#Predicting the test values
y_pred_m = predict(multi, newdata = test_set)

#Performance metrics
#install.packages('Metrics')
library(Metrics)

mae_m = mae(test_set$G3, y_pred_m)
rmse_m = rmse(test_set$G3, y_pred_m)

#Decision tree
library(rpart)

dt = rpart(formula = G3 ~ ., data = train_set,
           control = rpart.control(minsplit = 3))

#Predicting the test values
y_pred_dt = predict(dt, newdata = test_set)

#Performance metrics
mae_dt = mae(test_set$G3, y_pred_dt)
rmse_dt = rmse(test_set$G3, y_pred_dt)

#Random forest
library(randomForest)

set.seed(123)
rf = randomForest(formula = G3 ~ ., data = train_set,
                  ntree = 100)

#Predicting the test values
y_pred_rf = predict(rf, newdata = test_set)

#Performance metrics
mae_rf = mae(test_set$G3, y_pred_rf)
rmse_rf = rmse(test_set$G3, y_pred_rf)

#Saving the model
saveRDS(rf, file = "rf.rda")