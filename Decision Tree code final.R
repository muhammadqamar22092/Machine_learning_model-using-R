# Load required libraries
library(rpart)
library(caret)
library(rpart.plot)
library(pROC)
library(PRROC)
library(MASS)
# Read the dataset
data <- read.csv("file_1.csv")
# Convert 'Attrition' to a factor with valid levels
data$Attrition <- as.factor(data$Attrition)
factor_columns <- sapply(data, is.character)
data[factor_columns] <- lapply(data[factor_columns], as.factor)

# Split the data into training and test sets
trainData <- data[1:1029, ]
testData <- data[1030:1470, ]

# Define ranges for minsplit and minbucket
minsplit_values <- seq(10, 100, by = 10)
minbucket_values <- seq(10, 80, by = 10)

# Initialize an empty data frame to store results
results <- data.frame(minsplit = integer(), minbucket = integer(), F1 = numeric())

# Directory for plots
plots_dir <- "tree_plots"
dir.create(plots_dir, showWarnings = FALSE)

# Function to calculate F1 score
calculateF1Score <- function(true_values, predictions) {
  confusionMatrix <- table(Predicted = predictions, Actual = true_values)
  precision <- confusionMatrix[2,2] / sum(confusionMatrix[2,])
  recall <- confusionMatrix[2,2] / sum(confusionMatrix[,2])
  f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  return(f1)
}

# Nested loop over minsplit and minbucket values
for (minsplit in minsplit_values) {
  for (minbucket in minbucket_values) {
    # Build the model
    model <- rpart(Attrition ~ ., data = trainData, method = "class", 
                   control = rpart.control(minsplit = minsplit, minbucket = minbucket))
    
    # Make predictions
    predictions <- predict(model, testData, type = "class")
    
    # Calculate F1 Score
    f1_score <- calculateF1Score(testData$Attrition, predictions)
    
    # Store the results
    results <- rbind(results, data.frame(minsplit = minsplit, minbucket = minbucket, F1 = f1_score))
    
    # Plot and save the tree
    rpart.plot(model)
  }
}
# Review the results
print(results)


# Find the best combination of minsplit and minbucket (based on highest F1 score)
best_model <- results[which.max(results$F1),]
print(best_model)

# Build the final decision tree model with the best minsplit and minbucket
final_model <- rpart(Attrition ~ ., data = trainData, method = "class", 
                     control = rpart.control(minsplit = 70,minbucket = 10))

# Print the final model summary
print(final_model)

rpart.plot(final_model)

# Predict the probability of having diabetes for each observation in the training data
Train_predicted_prob <- predict(final_model, trainData, type = "prob")[,2]
# Predict the probability of having diabetes for each observation in the test data
Test_predicted_prob <- predict(final_model, testData, type = "prob")[,2]
combine_predictions <- c(Train_predicted_prob,Test_predicted_prob)
write.csv(combine_predictions,file='Decision_tree_prediction2.csv')
#For Train Data
roc <- roc.curve(scores.class0 = Train_predicted_prob, weights.class0 = as.numeric(trainData$Attrition)-1,
                 curve = T)
print(roc)
plot(roc)

prcurve <- pr.curve(scores.class0 = Train_predicted_prob, #predicted probabilities
                    weights.class0 = as.numeric(trainData$Attrition)-1, #actual flag,
                    curve = T)
print(prcurve)
plot(prcurve)


#For Test Data

roc <- roc.curve(scores.class0 = Test_predicted_prob, weights.class0 = as.numeric(testData$Attrition)-1,
                 curve = T)
print(roc)
plot(roc)

prcurve <- pr.curve(scores.class0 = Test_predicted_prob, #predicted probabilities
                    weights.class0 = as.numeric(testData$Attrition)-1, #actual flag,
                    curve = T)
print(prcurve)
plot(prcurve)

#Metrics for Training Data
# Calculate ROC and find optimal threshold
roc_obj_tran <- roc(trainData$Attrition, Train_predicted_prob)
coords_tain <- coords(roc_obj_tran, "best", ret="threshold")
optimal_threshold_train <- coords_tain$threshold

# Classify based on optimal threshold
pred_class_train <- ifelse(Train_predicted_prob > optimal_threshold_train, 1, 0)

# Calculate metrics and store the confusion matrix
confusion_matrix_train <- table(trainData$Attrition, pred_class_train)
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
roc_obj_test <- roc(testData$Attrition, Test_predicted_prob)
coords_test <- coords(roc_obj_test, "best", ret="threshold")
optimal_threshold_test <- coords_test$threshold

# Classify based on optimal threshold
pred_class_test <- ifelse(Test_predicted_prob > optimal_threshold_test, 1, 0)

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

