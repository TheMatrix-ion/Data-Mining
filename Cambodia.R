library(dplyr)
library(jpeg)
library(tidyverse)
library(caret)


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

df <- read.csv(file = "Cambodia_data.csv",header = T)
df <- select(df, -c(Year_Month, Event, Formatted_Year_Month))
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

reg2 = lm(Deaths ~ PC1 + PC3 + PC4 + PC6 + PC7, data = df_regression)
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


