# 加载必要的库
library(glmnet)      # 用于岭回归
library(caret)       # 用于交叉验证
library(dplyr)       # 用于数据操作
library(readxl)      # 用于读取Excel文件
library(ggplot2)     # 用于绘制相关性图

# 1. 读取数据
data <- read_excel("/Users/yunya/Documents/0.35fd/asd/17network.xlsx")

# 2. 提取第5到38列（大脑网络加载矩阵）作为特征，第39到49列作为行为学指标
data_features <- data[, 5:38]    # 大脑网络加载矩阵（特征）
data_behaviors <- data[, 39:49]  # 行为学指标（目标）

# 3. 初始化结果存储
results_list <- list()  # 用于存储所有模型的结果
correlation_results <- data.frame(Behavior = character(), Correlation = numeric(), P_value = numeric(), stringsAsFactors = FALSE)

# 4. 循环遍历每个行为学指标进行训练
for (behavior in colnames(data_behaviors)) {
  
  # 提取当前行为学指标的相关数据
  current_data <- data.frame(data_features, Behavior = data_behaviors[[behavior]])
  
  # 5. 删除当前数据中包含 NA 的行
  current_data_cleaned <- current_data[complete.cases(current_data), ]
  
  # 更新特征和目标变量（去除缺失值后的数据）
  adjusted_features <- current_data_cleaned[, 1:ncol(data_features)]    # 更新后的特征数据
  data_behavior <- current_data_cleaned[, ncol(data_features) + 1]      # 当前行为学指标
  
  # 6. 设置交叉验证参数（2F-CV），此时的索引已经更新为删除缺失值后的数据
  train_control <- trainControl(method = "cv", number = 2, savePredictions = "final", 
                                index = createFolds(1:nrow(current_data_cleaned), k = 2))
  
  # 7. 选择 λ 参数并进行岭回归训练
  lambda_values <- c(1, 10, 100, 500, 1000, 5000, 10000, 15000, 20000)  # λ值范围
  
  # 8. 构建模型公式
  formula <- as.formula(paste("Behavior ~", paste(colnames(adjusted_features), collapse = "+")))
  
  # 9. 使用岭回归进行训练并选择最佳的 λ
  ridge_model <- train(formula, 
                       data = current_data_cleaned, 
                       method = "glmnet", 
                       trControl = train_control, 
                       tuneGrid = expand.grid(alpha = 0, lambda = lambda_values))  # alpha = 0 表示岭回归
  
  # 10. 提取最佳 λ 值
  best_lambda <- ridge_model$bestTune$lambda
  print(paste("Best lambda for", behavior, ": ", best_lambda))
  
  # 11. 预测结果
  predictions <- predict(ridge_model, newdata = current_data_cleaned)
  
  # 12. 计算预测结果与真实值的相关性和 p 值
  correlation_test <- cor.test(predictions, data_behavior)  # 计算相关性和 p 值
  
  # 获取相关性和 p 值
  correlation_value <- correlation_test$estimate
  p_value <- correlation_test$p.value
  
  # 13. 存储相关性结果和 p 值
  correlation_results <- rbind(correlation_results, data.frame(Behavior = behavior, Correlation = correlation_value, P_value = p_value))
  
  # 14. 绘制相关性图
  plot <- ggplot(data.frame(Predicted = predictions, Actual = data_behavior), aes(x = Actual, y = Predicted)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    ggtitle(paste("Prediction vs Actual for", behavior, "\nCorrelation: ", round(correlation_value, 2), ", p-value: ", round(p_value, 4))) +
    xlab("Actual Values") +
    ylab("Predicted Values") +
    theme_minimal() 
    #scale_x_continuous(limits = c(0, NA)) +  # x轴从0开始
    #scale_y_continuous(limits = c(0, NA))    # y轴从0开始
  
  # 保存相关性图
  ggsave(paste0("/Users/yunya/Documents/0.35fd/asd/plot/plot_", behavior, "_correlation_plot.png"), plot = plot)
}

# 15. 查看并保存相关性结果
print(correlation_results)
write.csv(correlation_results, "/Users/yunya/Documents/0.35fd/asd/correlation_results.csv", row.names = FALSE)