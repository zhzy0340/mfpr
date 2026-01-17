library(ggpubr)
library(dplyr)
library(reshape2)

# 1. 读取Excel文件
setwd("/Users/yunya/Documents/0.35fd/asd")   # 修改工作目录
inputFile = "input.txt"      # 输入文件
outFile = "violinplot.pdf"

# 读取输入文件
rt = read.table(inputFile, sep = "\t", header = T, check.names = F, row.names = 1)
x = colnames(rt)[1]
colnames(rt)[1] = "Type"

# 转换成长格式
data = melt(rt, id.vars = c("Type"))
colnames(data) = c("Type", "Gene", "Expression")
data$Type <- as.factor(data$Type)

# Wilcoxon 检验结果
wilcox_results <- data %>%
  group_by(Gene) %>%
  summarise(
    w_value = wilcox.test(Expression ~ Type)$statistic,
    p_value = wilcox.test(Expression ~ Type)$p.value
  )

# 绘制分组小提琴图
p = ggviolin(data, x = "Gene", y = "Expression", fill = "Type", 
             ylab = "Value", xlab = "", 
             legend.title = x, 
             palette = c("#FF7F7F", "skyblue"),
             add = "boxplot",   # 可以叠加 boxplot 更直观
             add.params = list(width = 0.2)) +
  rotate_x_text(60) +
  theme(axis.text.x = element_text(size = 8))

# 添加显著性标记（Wilcoxon）
p1 = p + stat_compare_means(aes(group = Type),
                            method = "wilcox.test",
                            label = "p.signif",
                            symnum.args = list(cutpoints = c(0, 0.001, 0.05, 0.1, 1),
                                               symbols = c("***", "**", "*", " ")),
                            size = 3)

# 为每个基因的最大值添加 W 值标记，且仅在 p < 0.1 的地方显示
p1 = p1 + geom_text(data = wilcox_results %>% filter(p_value < 0.1),
                    aes(x = Gene, 
                        y = max(data$Expression[data$Gene == Gene]) + 0.2, 
                        label = sprintf("W = %.2f", w_value)), 
                    size = 3, color = "black", vjust = 0)

# 输出 PDF
pdf(file = outFile, width = 6, height = 5)
print(p1)
dev.off()