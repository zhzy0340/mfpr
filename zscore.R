library(readxl)
library(writexl)
library(mfp)
library(tidyverse)
library(reticulate)
library(dplyr)
library(readr)
setwd(dir="/Users/yunya/Documents/0.35fd/asd")
data_asd <- read_excel("asdf.xlsx")
# 设置主工作目录
base_dir <- "/Users/yunya/Documents/0.35fd"
setwd(dir=base_dir)
# data_asd0 <- read_excel("asd.xlsx")
# # 随机选择 200 条数据
# set.seed(145)  # 设置随机种子，确保结果可重复
# data_asd <- data_asd0[sample(nrow(data_asd0), 200), ]
data_td <- read.csv("comparison_set.csv")
# 
# prediction_list <- NULL
# z_score_list <- NULL
# #协调多站点训练数据
# source_python("combatGAM_Python4R.py")
source_python("combatGAM_Python4R.py")
#剔除只在协变量中站点只出现了一次的数据
# 使用dplyr的管道操作
#-------------asd处理----------
data_asd<- data_asd %>%
  group_by(SITE) %>%
  filter(n() > 1) %>%
  ungroup()
write.csv(data_asd,"data_oriasd.csv", row.names = FALSE)

#协变量的个数必须>=2，不然函数combatGAM_Python4R_function报错
covars_temp = data_asd[c("SITE","AGE")]
data_temp = data_asd[,c(5:ncol(data_asd))]
write.csv(covars_temp[,1:2],"rsite/covars_temp.csv", row.names = FALSE) # save temporary covariates
write.csv(data_temp,"rsite/data_temp.csv", row.names = FALSE) # save temporary data
# 
adjustedData_model <- combatGAM_Python4R_function('./rsite/')
data_harmonized <- data.frame(adjustedData_model[[1]])

data_ASD <- data.frame(matrix(0, nrow = nrow(data_asd), ncol = 203))
data_ASD[, c(1, 2)] = covars_temp
data_ASD[, 3] = data_asd["FD"]
data_ASD[, c(4:203)] = data_harmonized
names(data_ASD) <- colnames(data_asd[c(3, 2, 4, 5:204)])
write.csv(data_asd,"data_asd00000.csv", row.names = FALSE)
write.csv(data_ASD,"data_ASD.csv", row.names = FALSE)

# 中心化数据
rowMeanasd <- NULL
data_ASD_centered <- data_ASD
for (i in 1:nrow(data_ASD)) {
  # 选择当前行从第三列开始的数值数据，并确保它是数值型
  row_data <- as.numeric(data_ASD[i, 4:ncol(data_ASD)])
  
  # 计算当前行的均值
  rowMeanasd <- mean(row_data, na.rm = TRUE)  # 使用 na.rm = TRUE 忽略缺失值
  
  # 中心化当前行的数值数据
  data_ASD_centered[i, 4:ncol(data_ASD)] <- row_data - rowMeanasd  # 中心化当前行数据
}

write.csv(rowMeanasd, "rowMean_asd-all.csv", row.names = FALSE)
write.csv(data_ASD_centered,"data_ASD_centered-all.csv", row.names = FALSE)

#-------------TD处理----------
# 使用dplyr的管道操作
data_td<- data_td %>%
  group_by(SITE) %>%
  filter(n() > 1) %>%
  ungroup()
write.csv(data_td,"data_oritd.csv", row.names = FALSE)


#协变量的个数必须>=2，不然函数combatGAM_Python4R_function报错
covars_temp = data_td[c("SITE","AGE")]
data_temp = data_td[,c(5:ncol(data_td))]
write.csv(covars_temp[,1:2],"rsite/covars_temp.csv", row.names = FALSE) # save temporary covariates
write.csv(data_temp,"rsite/data_temp.csv", row.names = FALSE) # save temporary data
# 
adjustedData_model <- combatGAM_Python4R_function('./rsite/')
data_harmonized <- data.frame(adjustedData_model[[1]])
data_TD <- data.frame(matrix(0, nrow = nrow(data_td), ncol = 203))
data_TD[, c(1, 2)] = covars_temp
data_TD[, 3] = data_td["FD"]
data_TD[, c(4:203)] = data_harmonized
names(data_TD) <- colnames(data_td[c(3, 2,4,5:204)])
write.csv(data_TD,"data_TD.csv", row.names = FALSE)

# 中心化数据
rowMeantd <- NULL
data_TD_centered <- data_TD
for (i in 1:nrow(data_TD)) {
  # 选择当前行从第三列开始的数值数据，并确保它是数值型
  row_data <- as.numeric(data_TD[i, 4:ncol(data_TD)])
  
  # 计算当前行的均值
  rowMeantd <- mean(row_data, na.rm = TRUE)  # 使用 na.rm = TRUE 忽略缺失值
  
  # 中心化当前行的数值数据
  data_TD_centered[i, 4:ncol(data_TD)] <- row_data - rowMeantd  # 中心化当前行数据
}

write.csv(rowMeantd, "rowMean_td.csv", row.names = FALSE)
write.csv(data_TD_centered,  "data_TD_centered.csv", row.names = FALSE)



for(fold in 1:10){
  # 设置fold目录
  fold_dir <- file.path(base_dir, paste0("fold_", fold))
  setwd(fold_dir)
  mfpModel_list <- readRDS("mfpModel_list.rds")
  prediction_list_asd <- NULL
  z_score_list_asd <- NULL
  prediction_list_td <- NULL
  z_score_list_td <- NULL
  regions_asd  <-  data_ASD_centered[,c(4:ncol(data_ASD_centered))]
  regions_asd[is.na(regions_asd)] <- 0
  regions_td  <-  data_TD_centered[,c(4:ncol(data_TD_centered))]
  regions_td[is.na(regions_td)] <- 0
  # 循环每个region进行预测和计算Z分数
  for (region in 1:ncol(regions_asd)){
    # 对于 ASD 数据的预测
    predicted_asd <- predict(mfpModel_list[[region]], newdata = data_ASD_centered[, c(2,3, region)])
    prediction_list_asd[[region]] <- data.frame(predicted_asd)
    
    # 计算ASD Z分数
    residuals_asd <- mfpModel_list[[region]]$residuals
    z_score_asd <- (regions_asd[, region] - predicted_asd) / sqrt(sum(residuals_asd^2) / length(residuals_asd))
    z_score_list_asd[[region]] <- z_score_asd
    
    # 对于 TD 数据的预测
    predicted_td <- predict(mfpModel_list[[region]], newdata = data_TD_centered[, c(2, 3,region)])
    prediction_list_td[[region]] <- data.frame(predicted_td)
    
    # 计算TD Z分数
    residuals_td <- mfpModel_list[[region]]$residuals
    z_score_td <- (regions_td[, region] - predicted_td) / sqrt(sum(residuals_td^2) / length(residuals_td))
    z_score_list_td[[region]] <- z_score_td
  }
  
  names(prediction_list_asd) <- names(regions_asd)
  names(z_score_list_asd) <- names(regions_asd)
  names(prediction_list_td) <- names(regions_td)
  names(z_score_list_td) <- names(regions_td)
  setwd("/Users/yunya/Documents/0.35fd/zscore")
  write.csv(z_score_list_asd,paste0("zscore_ASD_",fold,".csv"),row.names=FALSE)
  write.csv(z_score_list_td,paste0("zscore_TD_",fold,".csv"),row.names=FALSE)
}


  