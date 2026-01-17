# 加载所需的库
library(readxl)   # 用于读取Excel文件
library(cluster)   # 用于计算轮廓系数
library(openxlsx)  # 用于保存结果为Excel文件
library(ggplot2)   # 用于可视化
library(factoextra) # 用于可视化聚类结果

# 1. 读取Excel文件
data <- read_excel("/Users/yunya/Documents/0.35fd/asd/asd-matchf.xlsx")

# 2. 提取第四列到第203列作为特征
features <- data[, 5:204]

# 3. 数据标准化
features_scaled <- scale(features)

# 设置聚类数
k <- 2  # 聚类数，可以根据实际情况调整
nstart <- 25  # 设置nstart参数，即使用25次不同的初始聚类中心

# 4. 执行K-means聚类并进行多次运行
n_runs <- 10  # 执行10次K-means聚类
cluster_results <- list()  # 用于保存每次的聚类结果
centroids_results <- list()  # 用于保存每次的聚类中心
tot_withinss_values <- numeric(n_runs)  # 用于保存每次的总平方误差（SSE）
silhouette_scores <- numeric(n_runs)  # 用于保存每次的轮廓系数

cluster_labels_matrix <- matrix(NA, nrow = nrow(features), ncol = n_runs)  # 聚类标签矩阵

for (i in 1:n_runs) {
  # 执行K-means聚类
  kmeans_result <- kmeans(features_scaled, centers = k, nstart = nstart)
  
  # 保存聚类结果
  cluster_results[[i]] <- kmeans_result$cluster
  centroids_results[[i]] <- kmeans_result$centers  # 聚类中心
  
  # 保存聚类标签
  cluster_labels_matrix[, i] <- kmeans_result$cluster
  # 保存每次的总平方误差（SSE）
  tot_withinss_values[i] <- kmeans_result$tot.withinss
  
  # 计算轮廓系数
  silhouette_result <- silhouette(kmeans_result$cluster, dist(features_scaled))  # 计算轮廓系数
  silhouette_scores[i] <- mean(silhouette_result[, 3])  # 计算轮廓系数的平均值
}

# 5. 聚类标签重标定
relabel_cluster <- function(new_labels, centroids_results, i) {
  # 获取当前聚类中心
  new_centroids <- centroids_results[[i]]
  
  # 计算与其他所有聚类中心的距离
  distances <- matrix(NA, nrow = k, ncol = k)  # 存储距离矩阵
  for (j in 1:k) {
    for (l in 1:k) {
      distances[j, l] <- sum((new_centroids[j, ] - centroids_results[[1]][l, ])^2)
    }
  }
  
  # 基于距离矩阵对聚类标签进行重标定
  mapping <- apply(distances, 1, which.min)  # 找到最小距离的聚类标签
  relabeled <- mapping[new_labels]  # 根据映射重标定
  return(relabeled)
}

# 6. 将所有聚类结果重标定
for (i in 1:n_runs) {
  cluster_results[[i]] <- relabel_cluster(cluster_results[[i]], centroids_results, i)
}

# 7. 比较聚类标签的一致性
# 重新生成聚类标签矩阵
cluster_labels_matrix <- matrix(NA, nrow = nrow(features), ncol = n_runs)
for (i in 1:n_runs) {
  cluster_labels_matrix[, i] <- cluster_results[[i]]
}

# 8. 选择最优的聚类结果
best_run_index <- which.min(tot_withinss_values)  # 找到最小SSE对应的运行索引
best_cluster_labels <- cluster_results[[best_run_index]]  # 最优聚类标签
best_centroids <- centroids_results[[best_run_index]]  # 最优聚类中心

# 9. 输出最优聚类结果
cat("最优聚类运行是第", best_run_index, "次，具有最小的总平方误差（SSE）: ", tot_withinss_values[best_run_index], "\n")
cat("最优聚类的轮廓系数: ", silhouette_scores[best_run_index], "\n")

# 10. 聚类结果可视化（使用PCA降维）
# 使用PCA进行降维，以便在二维空间中查看聚类效果
pca_result <- prcomp(features_scaled)  # 主成分分析降维

# 将聚类标签添加到PCA结果
pca_df <- data.frame(PCA1 = pca_result$x[, 1], PCA2 = pca_result$x[, 2], Cluster = factor(best_cluster_labels))

# 可视化聚类结果
ggplot(pca_df, aes(x = PCA1, y = PCA2, color = Cluster)) +
  geom_point() +
  ggtitle("K-means Clustering with PCA")

# 11. 进行ANOVA分析，评估每个特征的贡献
anova_results <- sapply(data.frame(features_scaled), function(x) {
  aov_result <- aov(x ~ factor(best_cluster_labels), data = data.frame(x, Cluster = best_cluster_labels))
  summary(aov_result)[[1]]["Pr(>F)"][1]  # 提取P值
})

# 将anova_results转换为数据框，以便按P值排序
anova_results_df <- data.frame(Feature = names(anova_results), P_value = anova_results)

# 显示 p 值小于 0.05 的显著特征
significant_features <- anova_results_df[anova_results_df$P_value < 0.05, ]
print(significant_features)

# 12. 保存 ANOVA 结果为 Excel 文件
write.xlsx(anova_results_df, "/Users/yunya/Documents/0.35fd/kmeans2/anova_feature_contribution.xlsx")

# 13. 保存每次的聚类结果和聚类中心为 Excel 文件
# 将每次的聚类标签保存到一个数据框中
clustering_results <- data.frame(ID = data$ID, Cluster = cluster_results[[1]])  # 保存第一次聚类的标签为示例
for (i in 2:n_runs) {
  clustering_results[paste("Cluster_run_", i, sep = "")] <- cluster_results[[i]]
}

# 保存聚类标签到Excel文件
write.xlsx(clustering_results, "/Users/yunya/Documents/0.35fd/kmeans2/kmeans_clustering_results_stability.xlsx")

# 14. 保存最优聚类结果和聚类中心
# 保存最优聚类中心
write.xlsx(best_centroids, "/Users/yunya/Documents/0.35fd/kmeans2/kmeans_best_centroids.xlsx")

# 保存最优聚类标签
best_clustering_results <- data.frame(ID = data$ID, Cluster = best_cluster_labels)
write.xlsx(best_clustering_results, "/Users/yunya/Documents/0.35fd/kmeans2/kmeans_best_clustering_results.xlsx")