# 导入必要的库
library(readxl)
library(dplyr)
library(openxlsx)

# 读取Excel文件
file_path <- "/Users/yunya/Documents/0.35fd/fd/avg_pek.xlsx"
df <- read_excel(file_path)

# 检查数据结构
head(df)

# 假设数据结构是这样的：Prefix | Ses | Rest | Average_FD
# 根据Prefix和Ses列进行分组，计算Average_FD的平均值，并去掉Rest列
result <- df %>%
  group_by(Prefix, Ses) %>%
  summarise(Average_FD = mean(Average_FD, na.rm = TRUE)) %>%
  ungroup()

# 查看结果
head(result)

# 如果需要保存结果到新文件
output_path <- "/Users/yunya/Documents/0.35fd/fd/avg_pek_processed.xlsx"
write.xlsx(result, output_path)

# 提示保存完成
cat("结果已保存至：", output_path)