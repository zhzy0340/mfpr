############################
# 1. 设置工作目录
############################
setwd("/Users/yunya/Documents/0.35fd")

library(readxl)
library(dplyr)
library(tidyr)

############################
# 2. 读取数据
############################
label <- read.csv("labelname.csv")  
data <- read_excel("type1.xlsx")

############################
# 3. 提取数据
############################

# 脑区偏离分数（第6–205列）
deviation_scores <- data[, 6:205]

# 行为学指标（按你给的列号）
behavior_scores <- data[, c(206:208, 212:215, 223:226)]

# 确保为数值型
deviation_scores <- as.data.frame(lapply(deviation_scores, as.numeric))
behavior_scores  <- as.data.frame(lapply(behavior_scores, as.numeric))

############################
# 4. 初始化相关系数和 p 值矩阵
############################
cor_matrix <- matrix(
  NA,
  nrow = ncol(deviation_scores),
  ncol = ncol(behavior_scores)
)

p_matrix <- matrix(
  NA,
  nrow = ncol(deviation_scores),
  ncol = ncol(behavior_scores)
)

############################
# 5. 计算 Pearson 相关
############################
for (i in seq_len(ncol(deviation_scores))) {
  for (j in seq_len(ncol(behavior_scores))) {
    
    test_res <- cor.test(
      deviation_scores[, i],
      behavior_scores[, j],
      method = "pearson",
      use = "complete.obs"
    )
    
    cor_matrix[i, j] <- test_res$estimate
    p_matrix[i, j]   <- test_res$p.value
  }
}

############################
# 6. 整理为数据框
############################
cor_df <- data.frame(
  Broca_Region = colnames(deviation_scores),
  cor_matrix
)

p_df <- data.frame(
  Broca_Region = colnames(deviation_scores),
  p_matrix
)

colnames(cor_df)[-1] <- colnames(behavior_scores)
colnames(p_df)[-1]   <- colnames(behavior_scores)

############################
# 7. 保存完整结果
############################
write.csv(
  cor_df,
  "type1/pearson/correlation_results1.csv",
  row.names = FALSE
)

write.csv(
  p_df,
  "type1/pearson/p_value_results1.csv",
  row.names = FALSE
)

############################
# 8. 转成长表（关键步骤）
############################
cor_long <- cor_df %>%
  pivot_longer(
    cols = -Broca_Region,
    names_to = "Behavior",
    values_to = "Correlation"
  )

p_long <- p_df %>%
  pivot_longer(
    cols = -Broca_Region,
    names_to = "Behavior",
    values_to = "P_value"
  )

results_long <- left_join(
  cor_long,
  p_long,
  by = c("Broca_Region", "Behavior")
)

############################
# 9. 筛选 p < 0.05
############################
significant_results <- results_long %>%
  filter(P_value < 0.05)

############################
# 10. 保存显著结果
############################
write.csv(
  significant_results,
  "type1/pearson/significant_results_p005.csv",
  row.names = FALSE
)