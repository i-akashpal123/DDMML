# Load necessary libraries
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(gbm)
# Load the dataset in R
airbnb_data_R <- read.csv('new_air_bnb.csv')
str(airbnb_data_R)

colnames(airbnb_data_R)

# Display the first few rows of the dataset
head(airbnb_data_R)
# Load necessary libraries
library(dplyr)

# Drop the specified columns using dplyr::select()
airbnb_data_cleaned_R <- airbnb_data_R %>%
  dplyr::select(-house_rules, -license)
head(airbnb_data_cleaned_R)

# Check the result
head(airbnb_data_cleaned_R)
#new start

#new end
# Checking for missing values in each column
missing_values_R <- sapply(airbnb_data_R, function(x) sum(is.na(x)))

# Display the missing values
print(missing_values_R)
# Dropping columns with a high percentage of missing values
# Load the dplyr library if not already loaded


airbnb_data_cleaned_R <- airbnb_data_R %>% dplyr::select(-house_rules, -license)

# For columns like 'last review' and 'reviews per month', fill missing values
airbnb_data_cleaned_R$reviews.per.month <- ifelse(is.na(airbnb_data_cleaned_R$reviews.per.month), 0, airbnb_data_cleaned_R$reviews.per.month)

# Filling missing values in 'minimum nights' with the median
median_minimum_nights_R <- median(airbnb_data_cleaned_R$minimum.nights, na.rm = TRUE)
airbnb_data_cleaned_R$minimum.nights <- ifelse(is.na(airbnb_data_cleaned_R$minimum.nights), median_minimum_nights_R, airbnb_data_cleaned_R$minimum.nights)

# Displaying the cleaned dataset
head(airbnb_data_cleaned_R)

library(ggplot2)

# Converting 'price' to numeric after removing the dollar sign
airbnb_data_cleaned_R$price <- as.numeric(gsub("\\$", "", airbnb_data_cleaned_R$price))

# Visualizing the distribution of prices
ggplot(airbnb_data_cleaned_R, aes(x = price)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Airbnb Prices", x = "Price", y = "Frequency")

# Visualizing the relationship between price and number of reviews
ggplot(airbnb_data_cleaned_R, aes(x = price, y = number.of.reviews)) +
  geom_point(alpha = 0.5) +
  labs(title = "Price vs. Number of Reviews", x = "Price", y = "Number of Reviews")
library(dplyr)
library(caret)

# Selecting relevant features for the model
# Selecting relevant features for the model
model_data_R <- airbnb_data_cleaned_R %>%
  dplyr::select(price, `minimum.nights`, `number.of.reviews`, 
                `reviews.per.month`, `availability.365`, `room.type`, neighbourhood)


# Handling categorical variables - Encoding
# Example: Encoding 'room type' and 'neighbourhood'
model_data_R$room_type <- as.factor(model_data_R$room.type)
model_data_R$neighbourhood <- as.factor(model_data_R$neighbourhood)

# Applying label encoding
model_data_R$room_type <- as.numeric(as.factor(model_data_R$room.type))
model_data_R$neighbourhood <- as.numeric(as.factor(model_data_R$neighbourhood))

# Displaying the processed data ready for modeling
head(model_data_R)
library(caret)
library(randomForest)
library(gbm)
# Handling missing values - filling with median for numerical features
num_cols <- sapply(model_data_R, is.numeric)
model_data_R[num_cols] <- lapply(model_data_R[num_cols], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Assuming a synthetic binary target variable for demonstration purposes
set.seed(0)  # for reproducibility
model_data_R$cancellation <- sample(0:1, nrow(model_data_R), replace = TRUE)

# Splitting the dataset into training and testing sets
library(caret)
set.seed(42)  # for reproducibility
index <- createDataPartition(model_data_R$cancellation, p = 0.8, list = FALSE)
train_data <- model_data_R[index, ]
test_data <- model_data_R[-index, ]

# Training models and testing
# Logistic Regression model
logistic_model_R <- glm(cancellation ~ ., data = train_data, family = 'binomial')
# Check the distribution of logistic regression predictions


# Making predictions on the test set (probabilities)
logistic_prob_R <- predict(logistic_model_R, newdata = test_data, type = "response")

# Convert probabilities to class labels
logistic_pred_R <- ifelse(logistic_prob_R > 0.5, 1, 0)
table(logistic_pred_R)
# Calculate accuracy
logistic_accuracy_R <- mean(logistic_pred_R == test_data$cancellation)
print(paste("Logistic Regression Accuracy:", logistic_accuracy_R))
# Confusion Matrix for Logistic Regression
# Convert predictions to a factor with levels 0 and 1
logistic_pred_R_factor <- factor(logistic_pred_R, levels = c(0, 1))

# Convert actual values to a factor with levels 0 and 1
cancellation_factor <- factor(test_data$cancellation, levels = c(0, 1))

# Check levels
print(levels(logistic_pred_R_factor))
print(levels(cancellation_factor))


# ROC Curve for Logistic Regression
#roc_logistic <- roc(cancellation_factor, logistic_prob_R)
#auc_logistic <- auc(roc_logistic)
#plot(roc_logistic, main="ROC Curve for Logistic Regression", col="blue")
#legend("bottomright", legend=c(paste("AUC =", round(auc_logistic, 2))), col="blue", lwd=2)



# Making predictions on the test set (getting probabilities)
random_forest_model_R <- randomForest(cancellation ~ ., data = train_data)

random_forest_prob_R <- predict(random_forest_model_R, newdata = test_data, type = "response")

# Convert probabilities to class labels using a threshold (e.g., 0.5)
random_forest_pred_R <- ifelse(random_forest_prob_R > 0.5, 1, 0)

# Checking the distribution of predictions
print(table(random_forest_pred_R))
# Evaluating the model: Calculate and print the accuracy
random_forest_accuracy_R <- mean(random_forest_pred_R == test_data$cancellation)
print(paste("Random forest accuracy", random_forest_accuracy_R))




# Convert all categorical variables to factors
#categorical_columns <- sapply(train_data, is.character)
#train_data[categorical_columns] <- lapply(train_data[categorical_columns], as.factor)
#test_data[categorical_columns] <- lapply(test_data[categorical_columns], as.factor)


# Training the Logistic Regression model
logistic_model_R <- glm(cancellation ~ ., data = train_data, family = 'binomial')

# Making predictions on the test set (probabilities)
logistic_prob_R <- predict(logistic_model_R, newdata = test_data, type = "response")

# Convert probabilities to class labels using a threshold (e.g., 0.5)
logistic_pred_R <- ifelse(logistic_prob_R > 0.5, 1, 0)

# Checking the distribution of predictions
print(table(logistic_pred_R))

# Evaluating the model: Calculate and print the accuracy
logistic_accuracy_R <- mean(logistic_pred_R == test_data$cancellation)
print(paste("logoistic accuracy",logistic_accuracy_R))


# Load the necessary library
library(gbm)

# Training the Gradient Boosting model
# Convert all categorical variables to factors
categorical_columns <- sapply(train_data, is.character)
train_data[categorical_columns] <- lapply(train_data[categorical_columns], as.factor)
test_data[categorical_columns] <- lapply(test_data[categorical_columns], as.factor)

gradient_boosting_model_R <- gbm(cancellation ~ ., data = train_data, 
                                 distribution = "bernoulli", 
                                 n.trees = 100, 
                                 interaction.depth = 3,
                                 n.minobsinnode = 10)

# Making predictions on the test set (probabilities)
gradient_boosting_prob_R <- predict(gradient_boosting_model_R, newdata = test_data, 
                                    n.trees = 100, type = "response")

# Convert probabilities to class labels using a threshold (e.g., 0.5)
gradient_boosting_pred_R <- ifelse(gradient_boosting_prob_R > 0.5, 1, 0)

# Checking the distribution of predictions
print(table(gradient_boosting_pred_R))

# Evaluating the model: Calculate and print the accuracy
gradient_boosting_accuracy_R <- mean(gradient_boosting_pred_R == test_data$cancellation)
print(gradient_boosting_accuracy_R)

library(pROC)

library(caret)

library(caret)

# Ensure logistic_pred_R and test_data$cancellation are factors with the same levels
logistic_pred_R_factor <- factor(logistic_pred_R, levels = c(0, 1))
cancellation_factor <- factor(test_data$cancellation, levels = c(0, 1))
# Create the confusion matrix
confusion_matrix_logistic <- table(Predicted = logistic_pred_R_factor, Actual = cancellation_factor)
print(confusion_matrix_logistic)

# Calculate Precision, Recall, and F1 Score
true_positives <- confusion_matrix_logistic[2, 2]
false_positives <- confusion_matrix_logistic[1, 2]
false_negatives <- confusion_matrix_logistic[2, 1]

precision_logistic <- true_positives / (true_positives + false_positives)
recall_logistic <- true_positives / (true_positives + false_negatives)
f1_score_logistic <- 2 * (precision_logistic * recall_logistic) / (precision_logistic + recall_logistic)

# Print the metrics
print(paste("Precision:", precision_logistic))
print(paste("Recall:", recall_logistic))
print(paste("F1 Score:", f1_score_logistic))


# ROC Curve for Logistic Regression
roc_logistic <- roc(cancellation_factor, logistic_prob_R)
auc_logistic <- auc(roc_logistic)
plot(roc_logistic, main="ROC Curves", col="blue")
legend("bottomright", legend=c(paste("Logistic (AUC =", round(auc_logistic, 2), ")")), 
       col="blue", lwd=2)


#random forest evalution 
library(caret)

# Convert Random Forest predictions to a factor
random_forest_pred_R_factor <- factor(random_forest_pred_R, levels = c(0, 1))
# Create the confusion matrix for Random Forest
confusion_matrix_rf <- table(Predicted = random_forest_pred_R_factor, Actual = cancellation_factor)
print(confusion_matrix_rf)
# Calculate Precision, Recall, and F1 Score for Random Forest
true_positives_rf <- confusion_matrix_rf[2, 2]
false_positives_rf <- confusion_matrix_rf[1, 2]
false_negatives_rf <- confusion_matrix_rf[2, 1]
precision_rf <- true_positives_rf / (true_positives_rf + false_positives_rf)
recall_rf <- true_positives_rf / (true_positives_rf + false_negatives_rf)
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)
# Print the metrics for Random Forest
print(paste("Precision (Random Forest):", precision_rf))
print(paste("Recall (Random Forest):", recall_rf))
print(paste("F1 Score (Random Forest):", f1_score_rf))

# ROC Curve for Random Forest
# Make sure to generate random_forest_prob_R if not already done
roc_rf <- roc(cancellation_factor, random_forest_prob_R)
auc_rf <- auc(roc_rf)
lines(roc_rf, col="red")
legend("bottomright", legend=c(paste("Logistic (AUC =", round(auc_logistic, 2), ")"),
                               paste("Random Forest (AUC =", round(auc_rf, 2), ")")), 
       col=c("blue", "red"), lwd=2)
#gradient boosting evalution 
library(caret)

# Convert Gradients Boosting predictions to a factor
gradient_boosting_pred_R_factor <- factor(gradient_boosting_pred_R, levels = c(0, 1))
# Create the confusion matrix for Gradient Boosting
confusion_matrix_gb <- table(Predicted = gradient_boosting_pred_R_factor, Actual = cancellation_factor)
print(confusion_matrix_gb)
# Calculate Precision, Recall, and F1 Score for Gradient Boosting
true_positives_gb <- confusion_matrix_gb[2, 2]
false_positives_gb <- confusion_matrix_gb[1, 2]
false_negatives_gb <- confusion_matrix_gb[2, 1]
precision_gb <- true_positives_gb / (true_positives_gb + false_positives_gb)
recall_gb <- true_positives_gb / (true_positives_gb + false_negatives_gb)
f1_score_gb <- 2 * (precision_gb * recall_gb) / (precision_gb + recall_gb)

# Print the metrics for Gradient Boosting
print(paste("Precision (Gradient Boosting):", precision_gb))
print(paste("Recall (Gradient Boosting):", recall_gb))
print(paste("F1 Score (Gradient Boosting):", f1_score_gb))


# ROC Curve for Gradient Boosting
# Make sure to generate gradient_boosting_prob_R if not already done
roc_gb <- roc(cancellation_factor, gradient_boosting_prob_R)
auc_gb <- auc(roc_gb)
lines(roc_gb, col="green")
legend("bottomright", legend=c(paste("Logistic (AUC =", round(auc_logistic, 2), ")"),
                               paste("Random Forest (AUC =", round(auc_rf, 2), ")"),
                               paste("Gradient Boosting (AUC =", round(auc_gb, 2), ")")), 
       col=c("blue", "red", "green"), lwd=2)


