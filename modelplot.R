#-------------mfp结果图----------
# 设置工作目录
setwd("/Users/yunya/Documents/fc model-new")
label <- read.csv("labelname.csv")
labelname <- label$ROI.Name  # 提取ROI名称列
datamodel <- read.csv("data_select.csv")
dataparameter <- read.csv("parameter.csv")

# 提取P值和F值（TestStat列）
p_values <- dataparameter$PValue
F_values <- dataparameter$TestStat

# 加载保存的模型
mfpModel_list <- readRDS("mfpModel_list.rds")

# 创建一个空白的绘图区域
par(mfrow = c(2, 2))

# 创建一个函数来生成模型的预测值
setwd("/Users/yunya/Documents/fc model-new/plot")
generate_predictions <- function(model, age_range) {
  preds <- predict(model, newdata = data.frame(AGE = age_range), type = "response", se.fit = TRUE)
  
  # 提取预测值和标准误差
  pred_values <- preds$fit
  std_errors <- preds$se.fit
  
  # 使用正态分布计算置信区间
  z_value <- qnorm(0.975)  # 95%置信区间
  lower_bound <- pred_values - z_value * std_errors
  upper_bound <- pred_values + z_value * std_errors
  
  return(data.frame(AGE = age_range, Predicted = pred_values, Lower = lower_bound, Upper = upper_bound))
}

# 创建一个函数来绘制模型和原始数据
plot_model <- function(model, region_data, region_name, F_value, p_value) {
  # 生成模型预测
  age_range <- 6:18  # 使用6到18岁的整数范围
  predictions <- generate_predictions(model, age_range)
  
  # 使用ggplot绘制模型预测和置信区间
  p <- ggplot() +
    geom_point(data = region_data, aes(x = AGE, y = region_data[, 2]), color = "grey65") +  # 绘制原始数据点
    geom_line(data = predictions, aes(x = AGE, y = Predicted), color = "#1f77b4", lwd = 1) +  # 绘制预测值
    geom_ribbon(data = predictions, aes(x = AGE, ymin = Lower, ymax = Upper), fill = "#1f77b4", alpha = 0.3) +  # 绘制置信区间
    labs(title = region_name, y = "FCS") +  # 设置Y轴标签
    annotate("text", x = Inf, y = -0.1, label = paste("F =", round(F_value, 4), ", p =", round(p_value, 8)), hjust = 1, vjust = 0)
  
  # 保存图形到文件
  file_name <- file.path(paste0("plot_", gsub(" ", "_", region_name), ".png"))
  ggsave(filename = file_name, plot = p, width = 8, height = 6, dpi = 300)
}

# 绘制每个区域的模型
for (i in seq_along(mfpModel_list)) {
  if (!is.null(mfpModel_list[[i]])) {
    # 获取每个区域的数据
    region_data <- datamodel[, c(2, i + 1)]  # 选择AGE列和region列（即第i+1列）
    plot_model(mfpModel_list[[i]], region_data, labelname[i], F_values[i], p_values[i])
  }
}



setwd(dir="/Users/yunya/Documents/fc model-new")
# Load the saved models
prediction_list <- NULL
mfpModel_list <- readRDS("mfpModel_list.rds")
# 创建一个空白的绘图区域,图比较小
par(mfrow=c(2, 2))
# Create a function to generate predicted values from the model
setwd("/Users/yunya/Documents/fc model-new/plot-r")
generate_predictions <- function(model, age_range) {
  preds <- predict(model, newdata = data.frame(AGE = age_range), type = "response", se.fit = TRUE)
  #prediction_list[i] <- data.frame(preds + regionMean[i])
  #print(regionMean[i])
  # Extract predicted values and standard errors
  pred_values <- preds$fit
  std_errors <- preds$se.fit
  
  # Calculate the confidence interval using normal distribution
  z_value <- qnorm(0.975)  # 95% confidence interval
  lower_bound <- pred_values - z_value * std_errors
  upper_bound <- pred_values + z_value * std_errors
  
  return(data.frame(AGE = age_range, Predicted = pred_values, Lower = lower_bound, Upper = upper_bound))
}

# Create a function to plot the model and original data
plot_model <- function(model, datamodel, region_name, F_value, p_value) {
  # Generate predictions from the model
  age_range <- seq(min(datamodel$AGE), max(datamodel$AGE), length.out = 100)
  predictions <- generate_predictions(model, age_range)
  
  # Plot the model predictions with confidence interval
  p <- ggplot() +
    geom_point(data = datamodel, aes(x = AGE, y = region_temp), color = "grey65") +  # Plot the original data points with light grey color
    geom_line(data = predictions, aes(x = AGE, y = Predicted), color = "#FF9999", lwd=1) +  # Plot the predicted values
    geom_ribbon(data = predictions, aes(x = AGE, ymin = Lower, ymax = Upper), fill = "#FF9999", alpha = 0.3) +  # Plot the confidence interval
    labs(title = region_name, y = "FCS")+  # Set Y axis label to region name
    annotate("text", x = Inf, y = -0.1, label = paste("F =", round(F_value, 4), ", p =", round(p_value, 8)), hjust = 1, vjust = 0)# Set Y axis label to region name
  #print(p)
  # 保存绘图到文件
  file_name <- file.path(paste0("plot_", gsub(" ", "_", region_name), ".png"))
  ggsave(filename = file_name, plot = p, width = 8, height = 6, dpi = 300)
  
}

# Plot each region's model
for (i in seq_along(mfpModel_list)) {
  if (!is.null(mfpModel_list[[i]])) {
    plot_model(mfpModel_list[[i]], datamodel[,c(2,i+2)], labelname[i,], F_values[i], p_values[i])
  }
}






# 设置工作目录
setwd("/Users/yunya/Documents/fc model-new")
label <- read.csv("labelname.csv")
labelname <- label$ROI.Name  # 提取ROI名称列
datamodel <- read.csv("data_select.csv")
dataparameter <- read.csv("parameter.csv")

# 提取P值和F值（TestStat列）
p_values <- dataparameter$PValue
F_values <- dataparameter$TestStat

# 加载保存的模型
mfpModel_list <- readRDS("mfpModel_list.rds")

# 创建一个空白的绘图区域
par(mfrow = c(2, 2))

# 创建一个函数来生成模型的预测值
setwd("/Users/yunya/Documents/fc model-new/plot-r")
generate_predictions <- function(model, age_range) {
  preds <- predict(model, newdata = data.frame(AGE = age_range), type = "response", se.fit = TRUE)
  
  # 提取预测值和标准误差
  pred_values <- preds$fit
  std_errors <- preds$se.fit
  
  # 使用正态分布计算置信区间
  z_value <- qnorm(0.975)  # 95%置信区间
  lower_bound <- pred_values - z_value * std_errors
  upper_bound <- pred_values + z_value * std_errors
  
  return(data.frame(AGE = age_range, Predicted = pred_values, Lower = lower_bound, Upper = upper_bound))
}

# 创建一个函数来绘制模型和原始数据
plot_model <- function(model, region_data, region_name, F_value, p_value) {
  # 生成模型预测
  age_range <- 6:18  # 使用6到18岁的整数范围
  predictions <- generate_predictions(model, age_range)
  
  # 使用ggplot绘制模型预测和置信区间
  p <- ggplot() +
    geom_point(data = region_data, aes(x = AGE, y = region_data[, 2]), color = "grey65") +  # 绘制原始数据点
    geom_line(data = predictions, aes(x = AGE, y = Predicted), color = "#FF9999", lwd = 1) +  # 绘制预测值
    geom_ribbon(data = predictions, aes(x = AGE, ymin = Lower, ymax = Upper), fill ="#FF9999", alpha = 0.3) +  # 绘制置信区间
    labs(title = region_name, y = "FCS") +  # 设置Y轴标签
    annotate("text", x = Inf, y = -0.1, label = paste("F =", round(F_value, 4), ", p =", round(p_value, 8)), hjust = 1, vjust = 0)
  
  # 保存图形到文件
  file_name <- file.path(paste0("plot_", gsub(" ", "_", region_name), ".png"))
  ggsave(filename = file_name, plot = p, width = 8, height = 6, dpi = 300)
}

# 绘制每个区域的模型
for (i in seq_along(mfpModel_list)) {
  if (!is.null(mfpModel_list[[i]])) {
    # 获取每个区域的数据
    region_data <- datamodel[, c(2, i + 1)]  # 选择AGE列和region列（即第i+1列）
    plot_model(mfpModel_list[[i]], region_data, labelname[i], F_values[i], p_values[i])
  }
}

