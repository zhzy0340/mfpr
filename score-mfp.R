library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(mfp)

# 设置工作目录
setwd("/Users/yunya/Documents/fc model-new")

# 读取数据集
data1 <- read_excel("/Users/yunya/Documents/fc model-new/k1.xlsx")
data2 <- read_excel("/Users/yunya/Documents/fc model-new/k2.xlsx")

# 提取第211列到第218列指标
cols_to_extract <- 211:218

# 提取相关列并转换为长格式
convert_to_long <- function(data, cols_to_extract, age_column_name, dataset_name) {
  data_subset <- data[, cols_to_extract]
  age <- data[[age_column_name]]
  
  data_long <- data_subset %>%
    mutate(age = age) %>%
    pivot_longer(cols = -age, names_to = "indicator", values_to = "value") %>%
    mutate(dataset = dataset_name) %>%
    mutate(indicator = as.character(indicator))  # 确保 indicator 是字符型
  
  return(na.omit(data_long))  # 删除缺失值
}

# 转换成长格式并加上数据集名称
data1_long <- convert_to_long(data1, cols_to_extract, "AGE", "Data1")
data2_long <- convert_to_long(data2, cols_to_extract, "AGE", "Data2")

# 创建拟合结果存储列表
fit_data_list <- list()
model_info_list <- list()  # 用于存储模型信息

# 拟合函数
fit_model <- function(data, indicator, df, dataset_name) {
  data_indicator <- data %>% filter(indicator == indicator)
  model <- tryCatch({
    mfp(value ~ fp(age, df = df), data = data_indicator)
  }, error = function(e) NULL)
  
  if (is.null(model)) return(list(fit_data = data.frame(), model_summary = NULL))  # 如果拟合失败，返回空数据框
  
  new_data <- data.frame(age = seq(6, 18, length.out = 100))
  new_data$value <- predict(model, newdata = new_data)
  new_data$indicator <- indicator
  new_data$dataset <- dataset_name
  
  # 获取模型摘要
  model_summary <- summary(model)
  
  # 返回拟合数据和模型摘要
  return(list(fit_data = new_data, model_summary = model_summary))
}

# 对每个指标拟合并保存结果
for (indicator in unique(c(data1_long$indicator, data2_long$indicator))) {
  
  # 对Data1进行拟合
  if (indicator %in% data1_long$indicator) {
    model1_results <- fit_model(data1_long, indicator, df = 4, dataset_name = "Data1")
  } else {
    model1_results <- list(fit_data = data.frame(), model_summary = NULL)
  }
  
  # 对Data2进行拟合
  if (indicator %in% data2_long$indicator) {
    model2_results <- fit_model(data2_long, indicator, df = 4, dataset_name = "Data2")
  } else {
    model2_results <- list(fit_data = data.frame(), model_summary = NULL)
  }
  
  # 保存拟合数据
  fit_data_indicator <- bind_rows(model1_results$fit_data, model2_results$fit_data)
  fit_data_list[[indicator]] <- fit_data_indicator
  
  # 保存模型摘要信息到 model_info_list
  model_info_list[[indicator]] <- list(
    model1_summary = model1_results$model_summary,
    model2_summary = model2_results$model_summary
  )
}

# 将所有拟合数据合并
all_fit_data <- bind_rows(fit_data_list)

# 绘制每个指标的拟合图
ggplot(all_fit_data, aes(x = age, y = value, color = dataset, linetype = dataset)) +
  geom_line() +
  facet_wrap(~ indicator, scales = "free_y") +
  labs(title = "Comparison of Indicator Changes with Age (Fitted Curves)", x = "Age", y = "Indicator Value") +
  theme_minimal() +
  theme(legend.position = "top", panel.grid = element_blank())

