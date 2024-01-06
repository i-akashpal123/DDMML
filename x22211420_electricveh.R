library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(ModelMetrics)
library(e1071)  # For SVM

# Load the dataset
electric_veh_data <- read_csv("electric_veh_data_updated.csv")
# Basic information and descriptive statistics
print(str(electric_veh_data))
print(summary(electric_veh_data))
# Distribution of Electric Range
ggplot(electric_veh_data, aes(x=`Electric Range`)) +
  geom_histogram(bins=30, fill="blue", alpha=0.7) +
  labs(title="Distribution of Electric Range", x="Electric Range (miles)", y="Frequency")

# Boxplot for Electric Range by Vehicle Type
# Visualization 1: Distribution of Electric Vehicle Types
ggplot(electric_veh_data, aes(x = `Electric Vehicle Type`)) +
  geom_bar() +
  labs(title = 'Distribution of Electric Vehicle Types', x = 'Electric Vehicle Type', y = 'Count')

ggplot(electric_veh_data, aes(x=`Electric Vehicle Type`, y=`Electric Range`)) +
  geom_boxplot() +
  labs(title="Electric Range by Vehicle Type", x="Vehicle Type", y="Electric Range (miles)")
# Visualization 2: Relationship Between Model Year and Electric Range
ggplot(electric_veh_data, aes(x = `Model Year`, y = `Electric Range`)) +
  geom_point() +
  labs(title = 'Model Year vs Electric Range', x = 'Model Year', y = 'Electric Range (miles)')

# Visualization 3: Influence of Base MSRP on Electric Range
ggplot(electric_veh_data, aes(x = `Base MSRP`, y = `Electric Range`)) +
  geom_point() +
  labs(title = 'Base MSRP vs Electric Range', x = 'Base MSRP', y = 'Electric Range (miles)')
# Handling missing values
electric_veh_data_cleaned <- na.omit(electric_veh_data)

# Function to transform features
transform_features <- function(df) {
  df <- mutate(df, 
               Log.Base.MSRP = log(`Base MSRP` + 1),
               Model.Year.Squared = `Model Year`^2)
  return(df)
}

# Apply transformations
electric_veh_data_transformed <- transform_features(electric_veh_data_cleaned)

# Splitting the dataset into training and testing sets
set.seed(42)
split <- createDataPartition(electric_veh_data_transformed$`Electric Range`, p = 0.8, list = FALSE)
train_set <- electric_veh_data_transformed[split, ]
test_set <- electric_veh_data_transformed[-split, ]

# Convert categorical variables to factors in the combined data
train_set$Make <- factor(train_set$Make)
train_set$Model <- factor(train_set$Model)
train_set$`Electric Vehicle Type` <- factor(train_set$`Electric Vehicle Type`)
test_set$Make <- factor(test_set$Make, levels = levels(train_set$Make))
test_set$Model <- factor(test_set$Model, levels = levels(train_set$Model))
test_set$`Electric Vehicle Type` <- factor(test_set$`Electric Vehicle Type`, levels = levels(train_set$`Electric Vehicle Type`))
# Align the levels of categorical variables in the test set with those in the training set
aligned_test_set <- test_set %>%
  filter(Make %in% levels(train_set$Make)) %>%
  filter(Model %in% levels(train_set$Model)) %>%
  filter(`Electric Vehicle Type` %in% levels(train_set$`Electric Vehicle Type`))

# Building the linear regression model
model_formula <- `Electric Range` ~ Log.Base.MSRP + Model.Year.Squared + Make + Model + `Electric Vehicle Type` + `Model Year` + `Base MSRP`
linear_model <- train(model_formula, data = train_set, method = "lm")

# Make predictions on the aligned test set
aligned_predictions <- predict(linear_model, aligned_test_set)

# Calculate R-squared on aligned test set
if(length(aligned_predictions) == nrow(aligned_test_set)) {
  r2_aligned <- R2(aligned_predictions, aligned_test_set$`Electric Range`)
  print(r2_aligned)
} else {
  print("The number of predictions still does not match the number of observations in the aligned test set.")
}
# Function to calculate R-squared
calculate_r2 <- function(predictions, test_df) {
  R2(predictions, test_df$`Electric Range`)
}

# Function to calculate RMSE
calculate_rmse <- function(predictions, test_df) {
  sqrt(mean((test_df$`Electric Range` - predictions)^2))
}

# Function to calculate MAE
calculate_mae <- function(predictions, test_df) {
  mean(abs(test_df$`Electric Range` - predictions))
}

# Function to calculate Adjusted R-squared
calculate_adjusted_r2 <- function(model, test_df) {
  summary(model$finalModel)$adj.r.squared
}

# Function to print evaluation metrics
print_evaluation_metrics <- function(r2, rmse, mae, adj_r2) {
  print(list("R2" = r2, "RMSE" = rmse, "MAE" = mae, "Adjusted R2" = adj_r2))
}
# Make predictions on the aligned test set
aligned_predictions <- predict(linear_model, aligned_test_set)

# Calculate evaluation metrics
r2_aligned <- calculate_r2(aligned_predictions, aligned_test_set)
rmse_aligned <- calculate_rmse(aligned_predictions, aligned_test_set)
mae_aligned <- calculate_mae(aligned_predictions, aligned_test_set)
adjusted_r2_aligned <- calculate_adjusted_r2(linear_model, aligned_test_set)

# Print the evaluation metrics
print_evaluation_metrics(r2_aligned, rmse_aligned, mae_aligned, adjusted_r2_aligned)
# Residual Plot
plot(aligned_predictions, aligned_test_set$`Electric Range` - aligned_predictions,
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red")
# Histogram of Residuals
hist(aligned_test_set$`Electric Range` - aligned_predictions, 
     main = "Histogram of Residuals", 
     xlab = "Residuals")

# QQ Plot of Residuals
qqnorm(aligned_test_set$`Electric Range` - aligned_predictions)
qqline(aligned_test_set$`Electric Range` - aligned_predictions)
#install.packages("lmtest")
library(lmtest)
bptest(linear_model$finalModel)
summary(model_formula)

