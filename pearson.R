# 设置工作目录
setwd("/Users/yunya/Documents/0.35fd")

# 读取数据
label <- read.csv("labelname.csv")  # 标签文件
data <- read_excel("/Users/yunya/Documents/0.35fd/type2.xlsx")  # 主数据

# 提取脑区偏离分数（第5列到第204列）
deviation_scores <- data[, 6:205]

# 提取行为学指标得分（第211列到第218列）
behavior_scores <- data[, c(206:208,212:215,223:226)]

# 确保数据是数值型，避免因子型或字符型数据
deviation_scores <- as.data.frame(lapply(deviation_scores, as.numeric))
behavior_scores <- as.data.frame(lapply(behavior_scores, as.numeric))

# 初始化存储相关系数和p值的结果数据框
correlation_results <- data.frame(Broca_Region = colnames(deviation_scores))

# 初始化一个空矩阵来存储相关性系数和p值
cor_matrix <- matrix(NA, nrow = ncol(deviation_scores), ncol = ncol(behavior_scores))
p_value_matrix <- matrix(NA, nrow = ncol(deviation_scores), ncol = ncol(behavior_scores))

# 计算每个脑区与每个行为学指标之间的皮尔逊相关性和p值
for (i in 1:ncol(deviation_scores)) {
  for (j in 1:ncol(behavior_scores)) {
    # 计算相关性时忽略包含 NA 的行
    cor_result <- cor.test(deviation_scores[, i], behavior_scores[, j], method = "pearson", use = "complete.obs")
    
    # 提取相关系数和p值
    cor_matrix[i, j] <- cor_result$estimate  # 相关系数
    p_value_matrix[i, j] <- cor_result$p.value  # p值
  }
}

# 将相关性矩阵转为数据框
correlation_results <- cbind(correlation_results, cor_matrix)
p_value_results <- data.frame(p_value_matrix)  # 转换为数据框

# 给列名添加行为学指标名称
colnames(correlation_results)[2:(ncol(correlation_results))] <- colnames(behavior_scores)
colnames(p_value_results)[1:(ncol(p_value_results))] <- colnames(behavior_scores)

# 输出相关性系数到 CSV 文件
write.csv(correlation_results, "/Users/yunya/Documents/0.35fd/type2/pearson/correlation_results1.csv", row.names = FALSE)

# 输出 p 值到 CSV 文件
write.csv(p_value_results, "/Users/yunya/Documents/0.35fd/type2/pearson/p_value_result1.csv", row.names = FALSE)

# 筛选 p 值小于 0.05 的相关性结果
significant_results <- correlation_results[p_value_results$p_value < 0.05, ]
significant_p_values <- p_value_results[p_value_results$p_value < 0.05, ]

# 输出 p 值小于 0.05 的相关性结果到 CSV 文件
write.csv(significant_results, "/Users/yunya/Documents/0.35fd/type2/pearson/significant_correlation_results1.csv", row.names = FALSE)

# 输出 p 值小于 0.05 的 p 值到 CSV 文件
write.csv(significant_p_values, "/Users/yunya/Documents/0.35fd/type2/pearson/significant_p_value_results1.csv", row.names = FALSE)

# 打印相关性结果的部分数据
head(correlation_results)
head(p_value_results)