# 加载必要的库
library(readxl)
library(kernlab)  # ksvm模型
library(caret)  # 用于交叉验证和递归特征消除（RFE）
library(iml)    # 用于计算SHAP值
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

# 递归特征消除（RFE）与交叉验证（RFECV）
ctrl <- rfeControl(functions=rfFuncs, method="cv", number=5)  # 5折交叉验证
rfe_result <- rfe(X_train, y_train, sizes=c(1:10), rfeControl=ctrl)  # 尝试不同数量的特征

# 打印RFE结果
print(rfe_result)

# 最终选择的特征
selected_features <- rfe_result$optVariables
X_train_selected <- X_train[, selected_features]
X_test_selected <- X_test[, selected_features]

# 打印最终选择的特征
cat("Selected features:", selected_features, "\n")

# 将选择的特征保存为数据框
selected_features_df <- data.frame(Feature = selected_features)

# 保存为Excel文件
write_xlsx(selected_features_df, "Selected_Features.xlsx")

# 使用最优特征训练 ksvm 模型
svm_model <- ksvm(Cluster ~ ., data = dataresult[trainIndex,], kernel = "rbf")

# 模型评估
svm_pred <- predict(svm_model, newdata = X_test_selected)
cm <- confusionMatrix(svm_pred, y_test)

# 打印模型评估结果
print(cm)

# 创建预测函数，适配 ksvm 的 predict 方法
predict_function <- function(model, X) {
  kernlab::predict(model, X)
}

# 使用 iml 包来计算 SHAP 值
# 创建一个预测模型对象，注意传递到 iml 的是 ksvm 的模型对象
predictor <- Predictor$new(model = svm_model, data = X_train_selected, y = y_train, predict.fun = predict_function)

# 计算 SHAP 值
shapley <- Shapley$new(predictor, x.interest = X_test_selected[1, , drop = FALSE])

# 可视化 SHAP 值
shapley$plot()

# 可视化整体 SHAP 值（对所有测试样本的平均贡献）
shapley_values <- shapley$results
ggplot(shapley_values, aes(x = reorder(Feature, .value), y = .value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + 
  theme_minimal() +
  labs(title = "SHAP Values for Features", x = "Feature", y = "SHAP Value") +
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12, face = "bold"))