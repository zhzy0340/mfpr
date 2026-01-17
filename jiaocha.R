library(readxl)
library(writexl)
library(mfp)
library(tidyverse)
library(reticulate)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(caret)  # 添加 caret 包用于交叉验证
library(car)  # 确保加载 car 包
library(carData)  # 确保加载 car 包


setwd(dir="/Users/yunya/Documents/0.35fd/tc")
dataf<- read_excel("td.xlsx")
setwd(dir="/Users/yunya/Documents/0.35fd")
label <- read.csv("labelname.csv")
labelname <- label["ROI.Name"]

# 设置随机种子以保证结果可重复
set.seed(123)
# 随机选择行的索引
sample_indices <- sample(1:nrow(dataf), size = 250, replace = FALSE)

# 创建对比集（300条随机数据）
comparison_set <- dataf[sample_indices, ]
write.csv(comparison_set, file = "comparison_set.csv", row.names = FALSE)

# 创建剩余的数据集（重命名为data）
data <- dataf[-sample_indices, ]
# 设定随机种子
#set.seed(246)

# 准备交叉验证
folds <- createFolds(1:nrow(data), k = 10, list = TRUE)

# 存储每次的结果
results_list <- list()

for (fold in 1:10) {
  cat("Running fold", fold, "\n")
  
  # 创建文件夹以保存每次的模型和数据
  fold_dir <- paste0("fold_", fold)
  if (!dir.exists(fold_dir)) {
    dir.create(fold_dir)
  }
  
  # 分割数据集
  train_indices <- folds[[fold]]
  data_train <- data[-train_indices, ]
  data_test <- data[train_indices, ]
  write.csv(data_train, file.path(fold_dir, "data_train.csv"), row.names = FALSE)
  write.csv(data_test, file.path(fold_dir, "data_test.csv"), row.names = FALSE)
  
  # 训练数据处理
  outlierIndexList <- NULL
  for (region in 5:ncol(data_train)) {
    current_column <- unlist(data_train[, region])
    numeric_column <- suppressWarnings(as.numeric(current_column))
    if (any(is.na(numeric_column))) {
      cat("Column", region, "contains non-numeric data\n")
      next
    }
    
    mean_value <- mean(numeric_column, na.rm = TRUE)
    sd_value <- sd(numeric_column, na.rm = TRUE)
    lower_bound <- mean_value - 3 * sd_value
    upper_bound <- mean_value + 3 * sd_value
    outliers <- which(numeric_column < lower_bound | numeric_column > upper_bound)
    outlierIndexList <- c(outlierIndexList, outliers)
  }
  
  outlierIndexList <- unique(unlist(outlierIndexList))
  data_train <- data_train[-outlierIndexList, ]
  
  # 协调多站点训练数据
  source_python("combatGAM_Python4R.py")
  data_train <- data_train %>%
    group_by(SITE) %>%
    filter(n() > 1) %>%
    ungroup()
  
  covars_temp = data_train[c("SITE","AGE")]
  data_temp = data_train[, c(5:ncol(data_train))]
  write.csv(covars_temp[,1:2],"rsite/covars_temp.csv", row.names = FALSE) # save temporary covariates
  write.csv(data_temp,"rsite/data_temp.csv", row.names = FALSE) # save temporary data
  
  write.csv(covars_temp[,1:2], file.path(fold_dir, "covars_temp.csv"), row.names = FALSE)
  write.csv(data_temp, file.path(fold_dir, "data_temp.csv"), row.names = FALSE)
  
  adjustedData_model <- combatGAM_Python4R_function('./rsite/')
  data_harmonized <- data.frame(adjustedData_model[[1]])
  
  data_training <- data.frame(matrix(0, nrow = nrow(data_train), ncol = 203))
  data_training[, c(1, 2)] = covars_temp
  data_training[, 3] = data_train["FD"]
  data_training[, c(4:203)] = data_harmonized
  names(data_training) <- colnames(dataf[c(3, 2, 4, 5:204)])
  write.csv(data_training, file.path(fold_dir, "data_training.csv"), row.names = FALSE)
  
  # 中心化数据
  rowMean <- NULL
  data_training_centered <- data_training
  for (i in 1:nrow(data_training)) {
    # 选择当前行从第4列开始的数值数据，并确保它是数值型
    row_data <- as.numeric(data_training[i, 4:ncol(data_training)])
    
    # 计算当前行的均值
    rowMean <- mean(row_data, na.rm = TRUE)  # 使用 na.rm = TRUE 忽略缺失值
    
    # 中心化当前行的数值数据
    data_training_centered[i, 4:ncol(data_training)] <- row_data - rowMean  # 中心化当前行数据
  }
  write.csv(rowMean, file.path(fold_dir, "rowMean_train.csv"), row.names = FALSE)
  write.csv(data_training_centered, file.path(fold_dir, "data_training_centered.csv"), row.names = FALSE)
  
  # 模型训练
  mfpModel_list <- NULL
  for (region in 4:ncol(data_training_centered)) {
    names(data_training_centered)[names(data_training_centered) == names(data_training_centered)[region]] <- "region_temp"
    mfpModel <- mfp(region_temp ~ fp(AGE, df = 4) + fp(FD, df=1), data = data_training_centered[, c(2,3,region)])
    mfpModel_list[[region - 3]] <- mfpModel
  }
  
  # 保存 mfpModel_list
  saveRDS(mfpModel_list, file = file.path(fold_dir, "mfpModel_list.RDS"))
  
  # 测试数据准备
  data_test <- data_test %>%
    group_by(SITE) %>%
    filter(n() > 1) %>%
    ungroup()
  
  covars_temp = data_test[c("SITE", "AGE")]
  data_temp = data_test[, c(5:ncol(data_test))]
  write.csv(covars_temp[,1:2],"rsite/covars_temp.csv", row.names = FALSE) # save temporary covariates
  write.csv(data_temp,"rsite/data_temp.csv", row.names = FALSE) # save temporary data
  
  write.csv(covars_temp[, 1:2], file.path(fold_dir, "covars_temp_test.csv"), row.names = FALSE)
  write.csv(data_temp, file.path(fold_dir, "data_temp_test.csv"), row.names = FALSE)
  
  adjustedData_model <- combatGAM_Python4R_function('./rsite/')
  data_harmonized <- data.frame(adjustedData_model[[1]])
  
  data_testing <- data.frame(matrix(0, nrow = nrow(data_test), ncol = 203))
  data_testing[, c(1, 2)] = covars_temp
  data_testing[, 3] = data_test["FD"]
  data_testing[, c(4:203)] = data_harmonized
  names(data_testing) <- colnames(dataf[c(3, 2, 4, 5:204)])
  write.csv(data_testing, file.path(fold_dir, "data_testing.csv"), row.names = FALSE)
  
  # 中心化测试数据
  rowMean_test <- NULL
  data_testing_centered <- data_testing
  for (i in 1:nrow(data_testing)) {
    # 选择当前行从第三列开始的数值数据，并确保它是数值型
    row_data <- as.numeric(data_testing[i, 4:ncol(data_testing)])
    
    # 计算当前行的均值
    rowMean_test <- mean(row_data, na.rm = TRUE)  # 使用 na.rm = TRUE 忽略缺失值
    
    # 中心化当前行的数值数据
    data_testing_centered[i, 4:ncol(data_testing)] <- row_data - rowMean_test  # 中心化当前行数据
  }
  write.csv(rowMean_test, file.path(fold_dir, "rowMean_test.csv"), row.names = FALSE)
  write.csv(data_testing_centered, file.path(fold_dir, "data_testing_centered.csv"), row.names = FALSE)
  
  # 初始化结果保存变量
  age_pvalues <- c()
  significant_models <- c()
  significant_joint_models <- c()
  age_joint_pvalues <- c()
  
  p_values_list <- data.frame()          # 每个模型和变量的 p 值
  joint_pvalue_list <- data.frame()      # 每个模型的联合 p 值
  
  # 遍历每个 region（从第3列开始）
  for (region in 4:ncol(data_training_centered)) {
    model_index <- region - 3
    model <- mfpModel_list[[model_index]]
    
    if (is.null(model)) {
      # 模型不存在时记录 NA
      age_pvalues <- c(age_pvalues, NA)
      age_joint_pvalues <- c(age_joint_pvalues, NA)
      next
    }
    
    # 提取模型摘要
    model_summary <- summary(model)
    coefficients <- model_summary$coefficients
    
    # 使用真实变量名来抓取 AGE 相关项
    age_vars <- grep("AGE", rownames(coefficients), value = TRUE)
    age_vars_hy <- grep("AGE", names(coef(model)), value = TRUE)
    
    # ----------- 逐个变量的 t 检验 ----------
    for (variable in age_vars) {
      p_val <- coefficients[variable, "Pr(>|t|)"]
      t_val <- coefficients[variable, "t value"]
      
      p_values_list <- rbind(p_values_list, data.frame(
        Model = model_index,
        Variable = variable,
        t = t_val,
        PValue = p_val
      ))
      
      age_pvalues <- c(age_pvalues, p_val)
      
      if (p_val < 0.05) {
        significant_models <- c(significant_models, model_index)
      }
    }
    
    # ----------- 联合F检验 ----------
    if (length(age_vars_hy) > 0) {
      hypothesis <- paste(age_vars_hy, "= 0")
      lh_test <- linearHypothesis(model, hypothesis)
      
      # 获取p值列的名称（自动判断F检验或卡方检验）
      pval_colname <- grep("Pr\\(>", colnames(lh_test), value = TRUE)
      
      if (length(pval_colname) > 0 && !is.null(lh_test[[pval_colname]][2])) {
        joint_p_val <- lh_test[[pval_colname]][2]
        age_joint_pvalues <- c(age_joint_pvalues, joint_p_val)
        
        # 提取对应统计量（F或Chisq）
        stat_colname <- grep("^(F|Chisq)$", colnames(lh_test), value = TRUE)
        stat_value <- if (length(stat_colname) > 0) lh_test[[stat_colname]][2] else NA
        
        joint_pvalue_list <- rbind(joint_pvalue_list, data.frame(
          Model = model_index,
          TestStat = stat_value,
          PValue = joint_p_val
        ))
        
        if (joint_p_val < 0.05) {
          significant_joint_models <- c(significant_joint_models, model_index)
        }
      } else {
        age_joint_pvalues <- c(age_joint_pvalues, NA)
      }
    }
  }
  
  # ----------- 结果统计与输出 ----------
  significant_summary <- data.frame(
    Model = unique(significant_models),
    Significant_Count = rep(length(unique(significant_models)))
  )
  
  # 写入 CSV 文件
  write.csv(p_values_list, file.path(fold_dir, paste0("fold_", fold, "_age_variables_p_values.csv")))
  write.csv(joint_pvalue_list, file.path(fold_dir, paste0("fold_", fold, "_age_joint_p_values.csv")))
  write.csv(significant_summary, file.path(fold_dir, paste0("fold_", fold, "_significant_models_summary.csv")))
  
  # 打印输出
  cat("Number of regions with significant AGE p-values (p < 0.05):", length(unique(significant_models)), "\n")
  cat("Significant model numbers:", unique(significant_models), "\n\n")
  
  cat("Number of regions with jointly significant AGE variables (p < 0.05):", length(unique(significant_joint_models)), "\n")
  cat("Significant joint model numbers:", unique(significant_joint_models), "\n")
  
  # # 计算F值和p值
  # F_values <- numeric(0)
  # p_values <- numeric(0)
  # 
  # for (i in 4:ncol(data_training_centered)) {
  #   model_summary <- mfpModel_list[[i - 3]]
  #   coefficients <- model_summary$coefficients
  #   age_vars <- grep("AGE", names(coefficients), value = TRUE)
  #   age_coefficient <- coefficients[age_vars]
  #   age_coefficients_value <- as.numeric(age_coefficient[1])
  #   
  #   if (length(age_coefficients_value) > 0) {
  #     predicted <- model_summary$fitted.values
  #     observed <- model_summary$y
  #     mean_observed <- mean(observed)
  #     
  #     SSR_age <- sum((predicted - mean_observed)^2)
  #     SSE_age <- sum(model_summary$residuals^2)
  #     
  #     p_age <- length(age_coefficients_value)
  #     df1_age <- p_age
  #     df2_age <- nrow(data_test) - p_age - 1
  #     
  #     F_value_age <- (SSR_age / df1_age) / (SSE_age / df2_age)
  #     F_values <- c(F_values, F_value_age)
  #     
  #     p_value_age <- pf(F_value_age, df1_age, df2_age, lower.tail = FALSE)
  #     p_values <- c(p_values, p_value_age)
  #   } else {
  #     cat("No age-related variables found in the model.\n")
  #   }
  # }
  # 
  # # FDR校正
  # p_values_fdr <- p.adjust(p_values, method = "fdr")
  # 
  # # 保存结果
  # results_list[[fold]] <- data.frame(F_values, p_values, p_values_fdr)
  # write.xlsx(results_list[[fold]], file.path(fold_dir, paste0("fold_", fold, "_results.xlsx")))
  # 
  # significant_count <- 0
  # significant_models <- numeric(0)
  # significant_count_fdr <- 0
  # significant_models_fdr <- numeric(0)
  # alpha <- 0.05  # 显著性水平
  # for (i in seq_along(F_values)) {
  #   if (p_values[i] < alpha) {
  #     cat("Model", i, "is significant.\n")
  #     significant_models <- c(significant_models, i)
  #     significant_count <- significant_count + 1
  #   } else {
  #     cat("Model", i, "is not significant.\n")
  #   }
  # }
  # for (i in seq_along(F_values)) {
  #   if (p_values_fdr[i] < alpha) {
  #     cat("Model", i, "is significant.\n")
  #     significant_models_fdr <- c(significant_models_fdr, i)
  #     significant_count_fdr <- significant_count_fdr + 1
  #   } else {
  #     cat("Model", i, "is not significant.\n")
  #   }
  # }
  # write.csv(significant_count, file.path(fold_dir, paste0("fold_", fold, "significant_model_count.csv")))
  # # 将显著的模型数量写入文档
  # write.csv(significant_models_fdr, file.path(fold_dir,paste0("fold_", fold, "significant_models_fdr.csv")))
  # # 将显著的模型数量写入文档
  # write.csv(significant_count_fdr, file.path(fold_dir,paste0("fold_", fold, "significant_model_count_fdr.csv")))
  #-------------测试集计算偏差----------
  # 创建空的数据框来存储每个模型的指标
  model_metrics <- data.frame(
    Model = character(),
    RMSE = numeric(),
    MSE = numeric(),
    R2 = numeric(),
    OSMSE = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 遍历每个模型并计算指标
  for (region in 4:ncol(data_testing_centered)) {
    model <- mfpModel_list[[region - 3]]
    
    # 获取预测值
    predicted <- predict(model, newdata = data_testing_centered[, c(2,3,region)])
    observed <- data_testing_centered[, region]
    
    # 计算 RMSE
    rmse <- sqrt(mean((predicted - observed)^2))
    
    # 计算 MSE
    mse <- mean((predicted - observed)^2)
    
    # 计算 R²
    r2 <- 1 - (sum((observed - predicted)^2) / sum((observed - mean(observed))^2))
    
    # 计算 OSMSE (假设存在标记异常值的 outlierIndexList)
    # 计算标准差
    sigma <- sd(observed)
    
    # 计算 OSMSE
    osmse <- mean(((observed - predicted) / sigma)^2)
    
    
    # 将每个模型的结果添加到数据框
    model_metrics <- rbind(model_metrics, data.frame(
      Model = paste("Model", region - 3),  # 模型名称
      RMSE = rmse,
      MSE = mse,
      R2 = r2,
      OSMSE = osmse
    ))
  }
  # 计算每个评价指标的平均值
  average_metrics <- data.frame(
    Model = "Average",  # 标记为平均值行
    RMSE = mean(model_metrics$RMSE),
    MSE = mean(model_metrics$MSE),
    R2 = mean(model_metrics$R2),
    OSMSE = mean(model_metrics$OSMSE)
  )
  
  # 将平均值添加到数据框
  model_metrics <- rbind(model_metrics, average_metrics)
  write.csv(model_metrics, file.path(fold_dir,paste0("fold_", fold, "model_metrics.csv")))
  
  
}

cat("Cross-validation finished. Results saved for each fold.\n")
