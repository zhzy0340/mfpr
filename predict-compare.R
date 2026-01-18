##################################################
# 0. 环境与依赖
##################################################
setwd("/Users/yunya/Documents/0.35fd")

library(readxl)
library(glmnet)
library(dplyr)
library(ggplot2)

set.seed(123)

##################################################
# 1. 读取数据
##################################################
data_all <- read_excel("asd/17network.xlsx")
data_t1  <- read_excel("asd/17/type1-17.xlsx")
data_t2  <- read_excel("asd/17/type2-17.xlsx")

##################################################
# 2. 指定列
##################################################
x_cols <- 5:38    # 脑区 z 分数
y_cols <- 39:49   # 行为学指标

##################################################
# 3. 预测函数（Prediction_r + RMSE + Relative_RMSE）
##################################################
predict_behavior_cv <- function(data, x_cols, y_cols, nfolds = 5) {
  
  X <- as.matrix(data[, x_cols])
  X <- apply(X, 2, as.numeric)
  
  Y <- as.data.frame(lapply(data[, y_cols], as.numeric))
  
  results <- data.frame()
  
  for (beh in colnames(Y)) {
    
    y <- Y[[beh]]
    keep <- complete.cases(X, y)
    
    n_valid <- sum(keep)
    
    out <- data.frame(
      Behavior = beh,
      Prediction_r = NA,
      RMSE = NA,
      Relative_RMSE = NA,
      N_used = n_valid,
      Status = NA
    )
    
    # 样本数不足
    if (n_valid < (nfolds + 1)) {
      out$Status <- "Too_few_samples"
      results <- rbind(results, out)
      next
    }
    
    X_use <- X[keep, ]
    y_use <- y[keep]
    
    # 行为为常数
    if (sd(y_use) == 0) {
      out$Status <- "Constant_behavior"
      results <- rbind(results, out)
      next
    }
    
    # Ridge 回归 + CV
    cvfit <- cv.glmnet(
      X_use,
      y_use,
      alpha = 0,
      nfolds = nfolds,
      standardize = TRUE
    )
    
    y_pred <- as.numeric(
      predict(cvfit, newx = X_use, s = "lambda.min")
    )
    
    # Prediction_r（若预测退化则为 NA）
    if (sd(y_pred) > 0) {
      out$Prediction_r <- suppressWarnings(cor(y_use, y_pred))
    }
    
    # RMSE
    rmse <- sqrt(mean((y_use - y_pred)^2))
    out$RMSE <- rmse
    
    # Relative RMSE（关键改进）
    out$Relative_RMSE <- rmse / sd(y_use)
    
    out$Status <- "OK"
    results <- rbind(results, out)
  }
  
  return(results)
}

##################################################
# 4. 分组预测
##################################################
res_all <- predict_behavior_cv(data_all, x_cols, y_cols)
res_all$Group <- "All_ASD"

res_t1 <- predict_behavior_cv(data_t1, x_cols, y_cols)
res_t1$Group <- "Type1"

res_t2 <- predict_behavior_cv(data_t2, x_cols, y_cols)
res_t2$Group <- "Type2"

##################################################
# 5. 合并结果
##################################################
results_all <- bind_rows(res_all, res_t1, res_t2)

##################################################
# 6. 保存结果
##################################################
write.csv(
  results_all,
  "asd/17/prediction_accuracy_r_rmse_relative.csv",
  row.names = FALSE
)

##################################################
# 7. 可视化 1：Prediction_r
##################################################
ggplot(results_all %>% filter(!is.na(Prediction_r)),
       aes(x = Behavior,
           y = Prediction_r,
           color = Group,
           group = Group)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y = "Prediction accuracy (Pearson r)",
    x = "Behavioral measures"
  )

##################################################
# 8. 可视化 2：Relative RMSE（推荐主图）
##################################################
ggplot(results_all,
       aes(x = Behavior,
           y = Relative_RMSE,
           color = Group,
           group = Group)) +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             linewidth = 0.8) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y = "Relative RMSE (RMSE / SD of behavior)",
    x = "Behavioral measures"
  )
##################################################
# 9. 计算 Prediction_r 对应的 p 值
##################################################

# 只对 Prediction_r 有值的行为计算
results_all$p_value <- NA

for (i in 1:nrow(results_all)) {
  r_val <- results_all$Prediction_r[i]
  n_val <- results_all$N_used[i]
  
  if (!is.na(r_val) && n_val > 2) {
    t_stat <- r_val * sqrt((n_val - 2) / (1 - r_val^2))
    p_val <- 2 * (1 - pt(abs(t_stat), df = n_val - 2))
    results_all$p_value[i] <- p_val
  }
}

# 转换为 -log10(p) 便于可视化
results_all$log10p <- -log10(results_all$p_value)
results_all$log10p[is.infinite(results_all$log10p)] <- NA  # 避免 Inf

##################################################
# 10. 可视化 -log10(p) 比较
##################################################
ggplot(results_all %>% filter(!is.na(log10p)),
       aes(x = Behavior,
           y = log10p,
           fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    y = "-log10(p) of Prediction_r",
    x = "Behavioral measures",
    fill = "Group"
  ) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red")