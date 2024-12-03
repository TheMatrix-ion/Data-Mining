install.packages("tidyverse")
install.packages("caret")
install.packages("datasets")
install.packages("readxl")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("xgboost")
install.packages("ggpubr")

  # 加载必要的 R 包
library(tidyverse)
library(readr)

# 读取数据
data <- read.csv("Ethiopia_data.csv")

# 查看数据前几行
head(data)

# 查看数据摘要
summary(data)


  # 检查缺失值
  sum(is.na(data)) # 返回数据集中缺失值总数

# 检查每列是否有缺失值
colSums(is.na(data))


  # 将 `Event` 转换为因子
  data$Event <- as.factor(data$Event)

# 查看因子的类别
levels(data$Event)


  # 统计不同事件的数量
  table(data$Event)

# 各数值变量的摘要统计
summary(data[, c("Deaths", "Injured", "Missing", "Houses.Destroyed", "Houses.Damaged")])


  library(ggplot2)

# 绘制事件类型的分布
ggplot(data, aes(x = Event)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Event Type Distribution", x = "Event", y = "Count")

# 绘制不同事件的死亡人数分布箱线图
ggplot(data, aes(x = Event, y = Deaths, fill = Event)) +
  geom_boxplot() +
  labs(title = "Deaths by Event Type", x = "Event", y = "Deaths") +
  theme(legend.position = "none")


  # 加载机器学习相关包
  library(caret)

# 划分训练集和测试集
set.seed(123)
train_indices <- createDataPartition(data$Event, p = 0.7, list = FALSE)
trainset <- data[train_indices, ]
testset <- data[-train_indices, ]


  library(randomForest)

# 构建随机森林模型
set.seed(123)
rf_model <- randomForest(Event ~ Deaths + Injured + Missing + Houses.Destroyed +
                           Houses.Damaged + Directly.affected + Indirectly.Affected +
                           Relocated + Evacuated, data = trainset, ntree = 500)

# 查看模型
print(rf_model)

# 变量重要性
varImpPlot(rf_model)


  # 预测
  predictions <- predict(rf_model, newdata = testset)

# 混淆矩阵
confusionMatrix(predictions, testset$Event)


  # 绘制误差率收敛图
  plot(rf_model)

# 查看重要变量的数值
importance(rf_model)

