library(fdrtool)
setwd(dir="/Users/yunya/Documents/0.35fd")
dataresult<-read_excel("modelresult.xlsx")

# 提取所需列

control_data <- dataresult$'positive_ratios_TD'
case_data <-dataresult$'positive_ratios_ASD'
# 假设您有两组数据：健康对照组和病例组，分别为 control_data 和 case_data
observed_difference <- dataresult$'positive_difference_ratio'
# 合并两组数据
total_data <- c(control_data, case_data)

# 定义一个函数执行基于组的置换检验
permutation_test <- function(total_data, control_size, n_permutations) {
  # 存储每次置换的差异指标值
  permutation_results <- numeric(n_permutations)
  
  # 执行置换检验
  for (i in 1:n_permutations) {
    # 对总数据标签进行随机排列
    permuted_labels <- sample(c(rep("control", control_size), rep("case", length(total_data) - control_size)))
    
    # 根据随机排列后的标签重新分配数据
    permuted_control_data <- total_data[permuted_labels == "control"]
    permuted_case_data <- total_data[permuted_labels == "case"]
    
    # 计算差异指标
    permutation_difference <- mean(permuted_case_data) - mean(permuted_control_data)
    
    # 将差异指标存储到结果向量中
    permutation_results[i] <- permutation_difference
  }
  
  # 返回结果
  return(list(permutation_results = permutation_results))
}

# 定义控制组大小和置换次数
control_size <- 200
n_permutations <- 10000

# 执行基于组的置换检验
result <- permutation_test(total_data, control_size, n_permutations)

permutation_results <- result$permutation_results

# 将结果保存到文件中
write.table(data.frame(permutation_results = permutation_results), 
            file = "positive_permutation_results.csv", sep = "\t", row.names = FALSE)
# 初始化一个空的向量，用于存储所有的 p 值
p_values <- numeric(length(observed_difference))

# 循环比较 observed_difference 中的每个元素
for (i in seq_along(observed_difference)) {
  # 计算 observed_difference 的第 i 个元素
  obs_diff <- observed_difference[i]
  
  # 计算 permutation_results 中大于 obs_diff 的比例
  p_values[i] <- sum(permutation_results > obs_diff) / length(permutation_results)
}

# 打印结果
print(p_values)
# #总体的置换检验
# o.observed_difference<- mean(control_data)- mean(case_data) 
# p_values_all<- mean(permutation_results >= o.observed_difference)
# print(p_values_all)
# 将 p 值保存到文件
write.table(data.frame(p_values), file = "p_p_values.csv", sep = "\t", row.names = FALSE, col.names = FALSE)
alpha <- 0.05
fdr_p_m<-rep(NA, length(200))
# 进行FDR校正
fdr_corrected_p_values <- p.adjust(p_values, method = "BH")
for (i in seq_along(p_values)) {
  if (fdr_corrected_p_values[i] < alpha) {
    fdr_p_m[i] <- fdr_corrected_p_values[i]
  }
}
# 将更新后的数据框写入 CSV 文件
write.csv(fdr_p_m, file = "fdrp_p_values.csv", row.names = FALSE)




# # 估计广义 Pareto 分布的参数
# shape <- length(permutation_results) / sum(log(permutation_results / min(permutation_results)))
# scale <- mean(permutation_results) / (shape - 1)
# 
# # 生成广义 Pareto 分布的累积分布函数（CDF）
# ppareto <- function(x, shape, scale) {
#   ifelse(x >= min(permutation_results), 1 - (scale / x) ^ shape, 0)
# }
# 
# # 创建数据框
# data_df <- data.frame(permutation_results = permutation_results, observed_difference = observed_difference, p_values = p_values_data)
# 
# # 绘制结果图
# ggplot(data_df, aes(x = permutation_results)) +
#   stat_ecdf(geom = "step") +
#   stat_function(fun = ppareto, args = list(shape = shape, scale = scale), color = "blue") +
#   geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
#   geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
#   labs(x = "Difference under null distribution", y = "Cumulative Probability") +
#   theme_minimal()
# 
# 
# 
# # 添加标记点
# data_df$marker <- ifelse(data_df$fdr_corrected_p_values < 0.05, "*", "o")
# 
# # 绘制结果图
# ggplot(data_df, aes(x = permutation_results)) +
#   stat_ecdf(geom = "step") +
#   stat_function(fun = ppareto, args = list(shape = shape, scale = scale), color = "blue") +
#   geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
#   geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
#   geom_point(aes(x = observed_difference, shape = marker), y = 0, color = "black") +
#   labs(x = "Permutation Results", y = "Cumulative Probability") +
#   scale_shape_manual(values = c("*" = 3, "o" = 1)) + # 设置形状
#   theme_minimal()


# 提取所需列

control_data <- dataresult$'negative_ratios_TD'
case_data <-dataresult$'negative_ratios_ASD'
# 假设您有两组数据：健康对照组和病例组，分别为 control_data 和 case_data
observed_difference <- dataresult$'negative_difference_ratio'
# 合并两组数据
total_data <- c(control_data, case_data)

# 定义一个函数执行基于组的置换检验
permutation_test <- function(total_data, control_size, n_permutations) {
  # 存储每次置换的差异指标值
  permutation_results <- numeric(n_permutations)
  
  # 执行置换检验
  for (i in 1:n_permutations) {
    # 对总数据标签进行随机排列
    permuted_labels <- sample(c(rep("control", control_size), rep("case", length(total_data) - control_size)))
    # 根据随机排列后的标签重新分配数据
    permuted_control_data <- total_data[permuted_labels == "control"]
    permuted_case_data <- total_data[permuted_labels == "case"]
    
    # 计算差异指标
    permutation_difference <- mean(permuted_case_data) - mean(permuted_control_data)
    
    # 将差异指标存储到结果向量中
    permutation_results[i] <- permutation_difference
  }
  
  # 返回结果
  return(list(permutation_results = permutation_results))
}

# 定义控制组大小和置换次数
control_size <- 200
n_permutations <- 10000

# 执行基于组的置换检验
result <- permutation_test(total_data, control_size, n_permutations)

permutation_results <- result$permutation_results

# 将结果保存到文件中
write.table(data.frame(permutation_results = permutation_results), 
            file = "negative_permutation_results.csv", sep = "\t", row.names = FALSE)
# 初始化一个空的向量，用于存储所有的 p 值
p_values <- numeric(length(observed_difference))

# 循环比较 observed_difference 中的每个元素
for (i in seq_along(observed_difference)) {
  # 计算 observed_difference 的第 i 个元素
  obs_diff <- observed_difference[i]
  
  # 计算 permutation_results 中大于 obs_diff 的比例
  p_values[i] <- sum(permutation_results > obs_diff) / length(permutation_results)
}


# 打印结果
print(p_values)
# #总体的置换检验
# o.observed_difference<- mean(control_data)- mean(case_data) 
# p_values_all<- mean(permutation_results >= o.observed_difference)
# print(p_values_all)
# 将 p 值保存到文件
write.table(data.frame(p_values), file = "n_p_values.csv", sep = "\t", row.names = FALSE, col.names = FALSE)
alpha <- 0.05
fdr_n_m<-rep(NA, length(200))
# 进行FDR校正
fdr_corrected_n_values <- p.adjust(p_values, method = "BH")
for (i in seq_along(p_values)) {
  if (fdr_corrected_n_values[i] < alpha) {
    fdr_n_m[i] <- fdr_corrected_n_values[i]
  }
}
# 将更新后的数据框写入 CSV 文件
write.csv(fdr_n_m, file = "fdrp_n_values.csv", row.names = FALSE)
