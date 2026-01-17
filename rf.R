# 加载必要的库
library(readxl)
library(caret)  # 用于交叉验证和特征选择
library(randomForest)  # 用于随机森林
library(ggplot2)  # 用于绘图
library(dplyr)  # 用于数据操作
library(writexl)  # 用于保存Excel文件

# 读取数据
dataresult <- read_excel("asd-match.xlsx")

# 提取数据特征和标签
X <- dataresult[, 6:205]  # 特征数据，从第6列到第205列
y <- as.factor(dataresult$Cluster)  # 标签，Cluster列

# 划分训练集和测试集（可以选择不划分进行交叉验证）
set.seed(123)  # 设置随机种子，确保可重复性
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# 设置交叉验证的控制参数
train_control <- trainControl(method = "cv", number = 5, search = "grid")

# 随机森林模型训练
rf_model <- train(X_train, y_train, method = "rf", trControl = train_control)

# 模型评估
rf_pred <- predict(rf_model, X_test)
cm <- confusionMatrix(rf_pred, y_test)

# 打印模型评估结果
print(cm)

# 提取特征重要性
feature_importance <- varImp(rf_model, scale = FALSE)

# 将特征重要性转化为数据框
importance_df <- as.data.frame(feature_importance$importance)
importance_df$Feature <- rownames(importance_df)

# 按照特征重要性排序
importance_df <- importance_df[order(-importance_df$Overall), ]

# 打印特征重要性排序表格
print(importance_df)

# 保存特征重要性排序表格到Excel文件
write_xlsx(importance_df, "Feature_Importance_Sorted.xlsx")

# 可视化前20个特征重要性排序
top_20_importance <- importance_df[1:20, ]

ggplot(top_20_importance, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "Top 20 Feature Importance Ranking", x = "Feature", y = "Importance") +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12, face = "bold"))