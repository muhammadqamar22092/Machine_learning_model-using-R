data <-read.csv("file_1.csv")
str(data)
print(names(data))
library(caret)
library(pROC)
library(MASS)
data$Attrition <- as.factor(data$Attrition)
# Convert categorical variables to factors
categorical_vars <- c("BusinessTravel", "Department","Age.Groups", "Distance", "EducationField", "Gender", "JobRole", "MaritalStatus","Rate.of.Promotion..Category.","OverTime", "Company.Tenure.Category" ,"Years.with.Manager_Category","Role.tenure.Category")
data[, categorical_vars] <- lapply(data[, categorical_vars], as.factor)

# Step 3: Split the data
trainData <- data[1: 1029,]
testData <- data[1030:1470,]
# Logistic Regression 
model <- glm(Attrition ~ ., data = trainData, family = "binomial")
summary(model)

# Generate predictions
Train_Predictions <- model$fitted.values
Test_Predictions <- predict(model, testData, type = "response")
All_Predictions <- c(Train_Predictions, Test_Predictions)
write.csv(All_Predictions, file = "final_file.csv")


model_2 <- stepAIC(model,direction = "backward")
summary(model_2)
Train_Predictions <- model_2$fitted.values
Test_Predictions <- predict(model_2, testData, type = "response")
All_Predictions <- c(Train_Predictions, Test_Predictions)
write.csv(All_Predictions, file = "final_file_2.csv")
library(pROC)
library(PRROC)
# Assuming 'Attrition' is a binary factor variable with levels 0 and 1
# If it's not binary, you may need to adjust accordingly
roc <- roc.curve(scores.class0 = model_2$fitted.values,
                 weights.class0 = as.numeric(trainData$Attrition) - 1,
                 curve = TRUE)
print(roc)
plot(roc)

roc_2 <- roc.curve(scores.class0 = Test_Predictions,
                   weights.class0 = as.numeric(testData$Attrition) - 1,
                   curve = TRUE)
print(roc_2)
plot(roc_2) 

Pr_curve <- pr.curve(scores.class0 = model_2$fitted.values,
                 weights.class0 = as.numeric(trainData$Attrition) - 1,
                 curve = TRUE)

print(Pr_curve)
plot(Pr_curve)

Pr_curve_2 <- pr.curve(scores.class0 = Test_Predictions,
                   weights.class0 = as.numeric(testData$Attrition) - 1,
                   curve = TRUE)
print(Pr_curve_2)
plot(Pr_curve_2)
#Metrics for Training Data
# Calculate ROC and find optimal threshold
roc_obj_tran <- roc(trainData$Attrition, Train_Predictions)
coords_tain <- coords(roc_obj_train, "best", ret="threshold")
optimal_threshold_train <- coords_tain$threshold

# Classify based on optimal threshold
pred_class_train <- ifelse(Train_Predictions > optimal_threshold_train, 1, 0)

# Calculate metrics and store the confusion matrix
confusion_matrix_train <- table(trainData$Attrition, pred_class)
print(confusion_matrix_train)

# Calculate Precision
train_data_precision <- confusion_matrix_train[2, 2] / sum(confusion_matrix_train[, 2])

# Calculate Recall
train_data_recall <- confusion_matrix_train[2, 2] / sum(confusion_matrix_train[2, ])

# Calculate Accuracy
train_data_accuracy <- sum(diag(confusion_matrix_train)) / sum(confusion_matrix_train)

# Calculate F1 Score
train_data_f1_score <- ifelse((train_data_precision + train_data_recall) > 0, 2 * (train_data_precision * train_data_recall) / (train_data_precision + train_data_recall), 0)

# Print the F1 Score
print(paste("F1 Score_train:", train_data_f1_score))

#Metrics fir Testing Data
# Calculate ROC and find optimal threshold
roc_obj_test <- roc(testData$Attrition, Test_Predictions)
coords_test <- coords(roc_obj_test, "best", ret="threshold")
optimal_threshold_test <- coords_test$threshold

# Classify based on optimal threshold
pred_class_test <- ifelse(Test_Predictions > optimal_threshold_test, 1, 0)

# Calculate metrics and store the confusion matrix
confusion_matrix_test <- table(testData$Attrition, pred_class_test)
print(confusion_matrix_test)

# Calculate Precision
precision_test_data <- confusion_matrix_test[2, 2] / sum(confusion_matrix_test[, 2])

# Calculate Recall
recall_test_data <- confusion_matrix_test[2, 2] / sum(confusion_matrix_test[2, ])

# Calculate Accuracy
accuracy_test_data <- sum(diag(confusion_matrix_test)) / sum(confusion_matrix_test)

# Calculate F1 Score
f1_score_test_data <- ifelse((precision_test_data + recall_test_data) > 0, 2 * (precision_test_data * recall_test_data) / (precision_test_data + recall_test_data), 0)

# Print the F1 Score
print(paste("F1 Score_test:", f1_score_test_data))

