# Data cleaning and visualisation: Yansong Fan
# PCA: Shupeng Wang
# Regression: Biyuan Luo
# Classification: Yaojie Zhang

# Loading necessary packages
library(dplyr)
library(jpeg)
library(tidyverse)
library(caret)
library(readr)
library(ggplot2)
library(randomForest)
library(xgboost)

# PCA————Shupeng Wang
pca_custom <- function(df){
  
  ccm <- cor(df)# Calculate the correlation coefficient matrix
  
  ev <- eigen(ccm)  # Calculate eigenvalues and eigenvectors
  
  eva <- ev$values  
  evv <- ev$vectors# Extract eigenvalues and eigenvectors
  
  pro_var <- eva / sum(eva)
  cum_pro <- cumsum(pro_var)  # Calculate variance contribution rate and cumulative variance contribution rate
  
  return(list(eigenvalues = eva, eigenvectors = evv, pro_var = pro_var, cum_pro = cum_pro))
}

df <- read.csv(file = "Mozambique_data.csv",header = T)
df <- select(df, -c(Formatted_Year_Month, Event))
cols_with_zero_sd <- apply(df, 2, sd) == 0
df <- df[, !cols_with_zero_sd]
df <- as.matrix(scale(df))

pca_result <- pca_custom(df)
pca_result$eigenvalues  # View eigenvalues
pca_result$pro_var      # Check the variance contribution rate
pca_result$cum_pro      # View the cumulative variance contribution rate

par(mar = c(6, 6, 2, 2))

plot(pca_result$eigenvalues, type = "b", cex = 2, cex.lab = 2, cex.axis = 2, lty = 2, lwd = 2,
     xlab = "PC number", ylab = "Eigenvalues")

#  Draw the cumulative variance contribution rate graph
plot(pca_result$cum_pro, type = "b", cex = 2, cex.lab = 2, cex.axis = 2, lty = 2, lwd = 2,
     xlab = "PC number", ylab = "Cum. Var. Cont.")

# Select the number of principal components
top_n <- 7

# Project data onto principal components
pca_result <- pca_custom(df)
df_pca <- df %*% pca_result$eigenvectors[, 1:top_n]

head(df_pca)
# PCA————Shupeng Wang

# Regression————Biyuan Luo
df <- as.data.frame(df)
df_regression <- data.frame(PC1 = df_pca[, 1], 
                            PC2 = df_pca[, 2], 
                            PC3 = df_pca[, 3], 
                            PC4 = df_pca[, 4], 
                            PC5 = df_pca[, 5], 
                            PC6 = df_pca[, 6], 
                            PC7 = df_pca[, 7], 
                            Deaths = df$Deaths) 


set.seed(10) 
training.samples <-df_regression$Deaths 
training.samples <- createDataPartition(1:length(df_regression$Deaths), p = 0.8, list = FALSE, group=1) 
train_data <- df_regression[training.samples,] 
test_data <- df_regression[-training.samples,]

reg1 = lm(Deaths ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7, data = df_regression)
summary(reg1)

reg2 = lm(Deaths ~ PC2 + PC3 + PC4 + PC5 + PC6 + PC7, data = df_regression)
summary(reg2)

# Train
pred1tr <- reg1 %>% predict(train_data) 
RMSE1 = RMSE(pred1tr, train_data$Deaths) 
R2_1 = R2(pred1tr, train_data$Deaths)

# Test
pred1test <- reg1 %>% predict(test_data) 
RMSE2 = RMSE(pred1test, test_data$Deaths) 
R2_2 = R2(pred1test, test_data$Deaths)

# Train
pred2tr <- reg2 %>% predict(train_data) 
RMSE3 = RMSE(pred1tr, train_data$Deaths) 
R2_3 = R2(pred2tr, train_data$Deaths)

# Test
pred2test <- reg2 %>% predict(test_data) 
RMSE4 = RMSE(pred1test, test_data$Deaths) 
R2_4 = R2(pred2test, test_data$Deaths)

# show tables
table <- matrix(c(RMSE1,RMSE2,R2_1,R2_2,RMSE3,RMSE4,R2_3,R2_4),ncol=4,byrow=TRUE) 
colnames(table) <- c("RMSE Train"," RMSE Test","R2 Train","R2 Test") 
rownames(table) <- c("Model 1","Model 2") 
RMSE_R2_table <- as.table(table) 
RMSE_R2_table
# Regression————Biyuan Luo

# Classification————Yaojie Zhang
# Load data
data <- read.csv("Cambodia_data.csv")

# View the first few rows of the data
head(data)

# View data summary
summary(data)

# Check for missing values
sum(is.na(data)) # Return the total number of missing values in the dataset

# Check for missing values in each column
colSums(is.na(data))

# Convert `Event` to a factor
data$Event <- factor(data$Event, levels = c("DROUGHT", "FLOOD", "STORM", "LIGHTNING", "FIRE", "RIVER BANK COLLAPSE", "PEST OUTBREAK", "EPIDEMIC"))

# View factor levels
levels(data$Event)

# Count the number of occurrences of each event type
table(data$Event)

# Summary statistics for numerical variables
summary(data[, c("Deaths", "Injured", "Missing", "Houses.Destroyed", "Houses.Damaged")])

# Plot the distribution of event types
ggplot(data, aes(x = Event)) +
  geom_bar(fill = "blue") +
  labs(title = "Event Type Distribution", x = "Event", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot the boxplot of deaths by event type
ggplot(data, aes(x = Event, y = Deaths, fill = Event)) +
  geom_boxplot() +
  scale_fill_manual(values = rep("blue", length(levels(data$Event)))) +
  labs(title = "Deaths by Event Type", x = "Event", y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Split the data into training and test sets
set.seed(123)
train_indices <- createDataPartition(data$Event, p = 0.7, list = FALSE)
trainset <- data[train_indices, ]
testset <- data[-train_indices, ]

# Ensure the training and test sets have the same column names
trainset <- trainset[, colnames(data)]
testset <- testset[, colnames(data)]

# Build the XGBoost model
# Convert data to matrix format (excluding target variable Event)
train_matrix <- model.matrix(~ . - Event, data = trainset)
test_matrix <- model.matrix(~ . - Event, data = testset)
train_label <- as.numeric(trainset$Event) - 1
test_label <- as.numeric(testset$Event) - 1

# Ensure feature names are consistent between training and test sets
train_matrix <- train_matrix[, colnames(train_matrix) %in% colnames(test_matrix)]
test_matrix <- test_matrix[, colnames(test_matrix) %in% colnames(train_matrix)]

# Create DMatrix objects
xg_train <- xgb.DMatrix(data = train_matrix, label = train_label)
xg_test <- xgb.DMatrix(data = test_matrix, label = test_label)

# Set parameters
params <- list(
  objective = "multi:softmax",
  num_class = length(levels(data$Event)),
  eval_metric = "mlogloss",
  max_depth = 6,
  eta = 0.3
)

# Train the XGBoost model
set.seed(123)
xg_model <- xgboost(params = params, data = xg_train, nrounds = 100, verbose = 0)

# Predict
xg_predictions <- predict(xg_model, xg_test)

# Convert predictions to factor format
xg_predictions_factor <- factor(xg_predictions, levels = 0:(length(levels(data$Event)) - 1), labels = levels(data$Event))

# Confusion matrix
xg_confusion_matrix <- confusionMatrix(xg_predictions_factor, testset$Event)
print(xg_confusion_matrix)

# Print model accuracy
xg_accuracy <- xg_confusion_matrix$overall["Accuracy"]
print(paste("XGBoost accuracy: ", round(xg_accuracy * 100, 2), "%", sep = ""))

# View important variables
xg_importance <- xgb.importance(model = xg_model)
print(xg_importance)
xgb.plot.importance(xg_importance)

# Build Random Forest model
set.seed(123)
rf_model <- randomForest(Event ~ Deaths + Injured + Missing + Houses.Destroyed +
                           Houses.Damaged + Directly.affected + Indirectly.Affected +
                           Relocated + Evacuated, data = trainset, ntree = 500, importance = TRUE)

# View Random Forest model
print(rf_model)

# Variable importance
varImpPlot(rf_model)

# Predict using Random Forest model
rf_predictions <- predict(rf_model, newdata = testset)

# Confusion matrix
rf_confusion_matrix <- confusionMatrix(rf_predictions, testset$Event)
print(rf_confusion_matrix)

# Print Random Forest model accuracy
rf_accuracy <- rf_confusion_matrix$overall["Accuracy"]
print(paste("Random Forest accuracy: ", round(rf_accuracy * 100, 2), "%", sep = ""))

# Compare XGBoost and Random Forest accuracy
accuracy_comparison <- data.frame(
  Model = c("XGBoost", "Random Forest"),
  Accuracy = c(round(xg_accuracy * 100, 2), round(rf_accuracy * 100, 2))
)

print("Comparison of accuracy of model XGBoost and Random Forest:")
print(accuracy_comparison)

# Plot the comparison of accuracy between XGBoost and Random Forest
accuracy_plot <- ggplot(accuracy_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "blue")) +
  labs(title = "Cambodia: The difference in accuracy between Random Forest and XGBoost", x = "Model", y = "Accuracy (%)") +
  theme_minimal()
ggsave("accuracy_comparison_plot.png", plot = accuracy_plot, width = 8, height = 6)
# Classification————Yaojie Zhang


