# 载入必要的包
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Metrics)  # 用于计算AIC/BIC

# 设置工作目录
setwd("/Users/yunya/Documents/fc model-new")

# 读取数据集
data1 <- read_excel("/Users/yunya/Documents/fc model-new/k1.xlsx")
data2 <- read_excel("/Users/yunya/Documents/fc model-new/k2.xlsx")

# 提取第211列到第218列指标
cols_to_extract <- 211:218

# 提取两个数据集中的相关列
data1_subset <- data1[, cols_to_extract]
data2_subset <- data2[, cols_to_extract]

# 假设数据集中有年龄这一列，列名为 'AGE'
age1 <- data1$AGE  # 读取data1中的年龄列
age2 <- data2$AGE  # 读取data2中的年龄列

# 将数据转换为长格式，以便用于ggplot绘图
data1_long <- data1_subset %>%
  mutate(age = age1) %>%
  gather(key = "indicator", value = "value", -age) %>%
  mutate(dataset = "Data1")

data2_long <- data2_subset %>%
  mutate(age = age2) %>%
  gather(key = "indicator", value = "value", -age) %>%
  mutate(dataset = "Data2")

# 合并两个数据集
combined_data <- bind_rows(data1_long, data2_long)

# 灵活拟合的函数：尝试不同的拟合方式
fit_model <- function(data) {
  models <- list()
  
  # 线性拟合
  models$linear <- lm(value ~ age, data = data)
  
  # 二次多项式拟合
  models$quadratic <- lm(value ~ poly(age, 2), data = data)
  
  # 三次多项式拟合
  models$cubic <- lm(value ~ poly(age, 3), data = data)
  
  # 指数拟合 (nls 非线性最小二乘法)
  models$exponential <- tryCatch(nls(value ~ a * exp(b * age), data = data, start = list(a = 1, b = 0.1)),
                                 error = function(e) NULL)
  
  # 对数拟合
  models$logarithmic <- tryCatch(lm(value ~ log(age), data = data),
                                 error = function(e) NULL)
  
  # 计算 AIC/BIC 值，选择最优模型
  model_metrics <- sapply(models, function(model) {
    if (is.null(model)) return(NA)
    c(AIC = AIC(model), BIC = BIC(model))
  })
  
  # 选择具有最低 AIC/BIC 值的模型
  best_model_name <- names(which.min(model_metrics[1, ]))
  best_model <- models[[best_model_name]]
  
  # 打印出最佳模型的详细信息
  print(paste("Best model for indicator", data$indicator[1], "is:", best_model_name))
  print(summary(best_model))  # 打印模型的详细信息
  
  # 返回最佳模型和其 AIC/BIC 值
  return(list(model = best_model, best_model_name = best_model_name, metrics = model_metrics))
}

# 拟合并选择最佳模型并计算 p 值
combined_data <- combined_data %>%
  group_by(indicator, dataset) %>%
  do({
    fit_result <- fit_model(.)
    best_model <- fit_result$model
    best_model_name <- fit_result$best_model_name
    
    # 获取 p 值（我们假设感兴趣的是系数的 p 值）
    p_value <- summary(best_model)$coefficients[2, 4]
    
    data.frame(p_value = p_value, best_model_name = best_model_name)  # 返回 p 值和最佳模型名称
  }) %>%
  ungroup()

# 合并p值回原始数据
combined_data <- left_join(combined_data, 
                           bind_rows(
                             data1_long %>% mutate(dataset = "Data1"),
                             data2_long %>% mutate(dataset = "Data2")
                           ), by = c("indicator", "dataset"))

# 绘制拟合图并标注p值和拟合方式
ggplot(combined_data, aes(x = age, y = value, color = dataset)) +
  geom_smooth(method = "lm", aes(group = dataset), formula = y ~ poly(x, 3), se = FALSE) +  # 默认三次多项式拟合
  facet_wrap(~ indicator, scales = "free_y") +  # 按指标分面展示
  geom_text(aes(x = 18, y = max(value), label = paste("p:", round(p_value, 3), "\nModel:", best_model_name)),
            vjust = -0.5, hjust = 1) +  # 在每个图中标注p值和模型名称
  labs(title = "Comparison of Indicator Changes with Age (Flexible Fit)", x = "Age", y = "Indicator Value") +
  xlim(6, 18) +  # 设置x轴范围为6到18
  theme_minimal() +
  theme(legend.position = "none",  # 去除图例
        panel.grid = element_blank())  # 去除网格线