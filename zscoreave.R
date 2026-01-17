# 设置工作目录  
setwd("/Users/yunya/Documents/0.35fd/zscore")
# 获取文件夹中所有以zscore_TD_开头的CSV文件  
# files <- list.files(pattern = "^zscore_TD_\\d+.csv$")    
# # 假设所有CSV文件的列结构相同，我们只需要读取第一个文件来获取列名  
# if (length(files) > 0) {  
#   first_file <- read.csv(files[1], header = TRUE)  
#   col_names <- names(first_file)  
# } else {  
#   stop("没有找到CSV文件。")  
# } 
# 读取第一个CSV文件以获取列名和行数  
first_file <- read.csv(list.files(pattern = "^zscore_TD_\\d+.csv$", full.names = TRUE)[1], header = TRUE)  
#first_file <- read.csv(list.files(pattern = "^zscore_ASD_\\d+.csv$", full.names = TRUE)[1], header = TRUE)  
col_names <- names(first_file)  
num_rows <- nrow(first_file)  

# 初始化一个列表来存储所有文件中相同列名的数据  
column_data <- list()

#datalie <- list()
#row_averages <- numeric(0)
# 遍历所有CSV文件，提取相同列名的数据  
 for (col_index in seq_along(col_names)) {
   datalie <- list()
   i<-1
   col_name <- col_names[col_index]  
   for (file in list.files(pattern = "^zscore_TD_\\d+.csv$", full.names = TRUE)) {  
   #for (file in list.files(pattern = "^zscore_ASD_\\d+.csv$", full.names = TRUE)) {  
     df <- read.csv(file, header = TRUE)  
     if (col_name %in% names(df)) {  
      # 如果当前文件包含此列，则提取该列的数据  
      datalie[[i]] <- df[[col_name]]
      i<-i+1
     } else {  
      # 如果列名不存在于当前文件中，添加NA值，确保长度与第一个文件的行数相同  
      datalie <- NULL
    }  
   }  
   column_data[[col_index]] <-datalie
}  

# 假设 column_data 是一个列表，其中每个元素是一个数据框或矩阵，并且它们都有相同的行数  


# 初始化一个空的数据框来保存结果  
result_df <- data.frame(matrix(ncol = length(col_names), nrow = nrow(first_file) ))  
names(result_df) <- col_names  
# 循环遍历 column_data 中的每个元素  
for (i in 1:200) {  
  # 获取当前元素  
  current_data <- data.frame(column_data[[i]])
  # 计算每行的平均值  
  row_means <- rowMeans(current_data)  
  
  # 将行平均值作为一个新列添加到结果数据框中  
  # 使用 paste 函数来创建列名，假设 col_names 是一个字符向量，用于生成列名  
  result_df[i] <- row_means  
}  

# 如果还没有 col_names，可以创建它们  
# col_names <- paste0("col", 1:200)  
# names(result_df) <- col_names  

# 将结果数据框写入新的 CSV 文件  
write.csv(result_df, "zscore_results_TD.csv", row.names = FALSE)
#write.csv(result_df, "zscore_results_ASD.csv", row.names = FALSE)


pratios_TD <- numeric(0)
nratios_TD <- numeric(0)
pratios_ASD <- numeric(0)
nratios_ASD <- numeric(0)
positive_ratios_TD <- list()
negative_ratios_TD <- list()
positive_ratios_ASD <- list()
negative_ratios_ASD <- list()
# 读取Z分数数据
z_score_asd <- read.csv("zscore_results_ASD.csv")
z_score_td <- read.csv("zscore_results_TD.csv")

# 计算每个区域Z分数大于2.6的比例
positive_ratios_TD <- colSums(z_score_td > 2.6) / nrow(z_score_td)
# 计算每个区域Z分数小于-2.6的比例
negative_ratios_TD <- colSums(z_score_td < (-2.6)) / nrow(z_score_td)
#一个人出现极端偏差的比例
# 对逻辑矩阵的每一行使用rowMeans()函数来计算满足条件的比例
row_negative_ratio <- rowMeans(z_score_td < (-2.6))
row_positive_ratio <- rowMeans(z_score_td > 2.6)
# 将结果写入文件
# 创建一个数据框
result_df <- data.frame(row_negative_ratio, row_positive_ratio)
# 将数据框写入CSV文件
write.csv(result_df, "result_TD.csv", row.names = TRUE)
#计算出现极端偏差的个体比例
pratios_TD <- (sum(row_positive_ratio != 0))/nrow(z_score_td)
nratios_TD <- (sum(row_negative_ratio != 0))/nrow(z_score_td)
# pratios_TD <- c(pratios_TD,pratio_TD)
# nratios_TD <- c(nratios_TD,nratio_TD)



# 计算每个区域Z分数大于2.6的比例
positive_ratios_ASD <- colSums(z_score_asd > 2.6) / nrow(z_score_asd)
# 计算每个区域Z分数小于2.6的比例
negative_ratios_ASD <- colSums(z_score_asd < (-2.6)) / nrow(z_score_asd)

#每个个体出现极端偏差的比例。
# 对逻辑矩阵的每一行使用rowMeans()函数来计算满足条件的比例
row_positive_ratio <- rowMeans(z_score_asd > 2.6)
row_negative_ratio <- rowMeans(z_score_asd < (-2.6))
# 输出结果
result_df <- data.frame(row_negative_ratio, row_positive_ratio)
# 将数据框写入CSV文件
write.csv(result_df,"result_ASD.csv", row.names = TRUE)
#计算出现极端偏差的个体比例
pratios_ASD <- (sum(row_positive_ratio != 0))/nrow(z_score_asd)
nratios_ASD <- (sum(row_negative_ratio != 0))/nrow(z_score_asd)



setwd("/Users/yunya/Documents/0.35fd/result-ratio")
write.csv(positive_ratios_ASD,file = "positive_ratios_ASD.csv", row.names = TRUE )
write.csv(negative_ratios_ASD,file = "negative_ratios_ASD.csv", row.names = TRUE )
write.csv(positive_ratios_TD,file = "positive_ratios_TD.csv", row.names = TRUE )
write.csv(negative_ratios_TD,file = "negative_ratios_TD.csv", row.names = TRUE )

ratio_data_frame <- data.frame(
  `P Ratios (ASD)` = pratios_ASD,
  `N Ratios (ASD)` = nratios_ASD,
  `P Ratios (TD)` = pratios_TD,
  `N Ratios (TD)` = nratios_TD
)
# 将数据框写入CSV文件，确保包含了列名
write.csv(ratio_data_frame, file = "people-ratio_data.csv", row.names = FALSE)