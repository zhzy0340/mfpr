# 载入必要的包
library(readxl)
library(ggplot2)
library(dplyr)

# 设置工作目录
setwd("/Users/yunya/Documents/fc model-new")

# 读取数据集
data1 <- read_excel("/Users/yunya/Documents/fc model-new/k1.xlsx")
data2 <- read_excel("/Users/yunya/Documents/fc model-new/k2.xlsx")

# 提取第211列到第218列指标
cols_to_extract <- 211:218

# 提取两个数据集中的相关列
data1_subset <- data1[, cols_to_extract]
data2_subset <- data2[, cols_to_extract]

# 假设数据集中有年龄这一列（假设为第10列），根据数据结构调整
age1 <- data1$AGE  # 调整为数据集中实际的年龄列名
age2 <- data2$AGE  # 调整为数据集中实际的年龄列名

# 将数据转换为长格式，以便用于ggplot绘图
data1_long <- data1_subset %>%
  mutate(age = age1) %>%
  gather(key = "indicator", value = "value", -age) %>%
  mutate(dataset = "Data1")

data2_long <- data2_subset %>%
  mutate(age = age2) %>%
  gather(key = "indicator", value = "value", -age) %>%
  mutate(dataset = "Data2")

# 合并两个数据集
combined_data <- bind_rows(data1_long, data2_long)

# 绘制每个指标随年龄变化的图
ggplot(combined_data, aes(x = age, y = value, color = dataset)) +
  geom_line() +
  facet_wrap(~ indicator, scales = "free_y") +  # 按指标分面展示
  labs(title = "Comparison of Indicator Changes with Age", x = "Age", y = "Indicator Value") +
  theme_minimal()