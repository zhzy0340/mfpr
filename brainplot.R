library(readxl)
library(writexl)
library(tidyverse)
library(reticulate)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(mgcv)
library(magrittr)
library(ggsegSchaefer)
library(sf)
library(ggplot2)
library(ggseg3d)
library(ggseg)
library(paletteer)
library(scales)
library(patchwork)
library(ggpubr)
library(extrafont)
library(showtext)
setwd(dir="/Users/yunya/Documents/0.35fd")
dataresult <- read_excel("modelresult.xlsx")


datadefault <- read_excel("default.xlsx")
# Enable this universe
options(repos = c(
  ggseg = 'https://ggseg.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages("ggsegSchaefer")

ggseg3d(atlas = schaefer17_200_3d, surface = "inflated") %>% 
  pan_camera("right lateral")
ggseg3d(atlas = schaefer17_200_3d, surface = "inflated") %>% 
  pan_camera("left lateral")

setwd("E:/bishe/grouplevel")
ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = mASD_FCS,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "") +
  #paletteer::scale_fill_paletteer_c("pals::ocean.matter", na.value="transparent", direction = -1, limits = c(0, .05), oob = squish) +
  #   theme(legend.position = "none")
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-0.026, 0.054), oob = squish) +
  theme(legend.text = element_text(family = "Arial", color = "black"))


ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = mTD_FCS,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-0.026, 0.054), oob = squish) +
  theme(legend.text = element_text(family = "Arial", color = "black"))

#-------------FCS----------
plot1 <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = mASD_FCS, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "FCS") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-0.026, 0.054), oob = squish) +
  theme(legend.position = "bottom", legend.title = element_text(family = "Arial", color = "black"), legend.text = element_text(family = "Arial", color = "black")) +
  ggtitle("mASD")

# Plot 2
plot2 <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = mTD_FCS, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "FCS") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-0.026, 0.054), oob = squish) +
  theme(legend.position = "bottom", legend.title = element_text(family = "Arial", color = "black"), legend.text = element_text(family = "Arial", color = "black")) +
  ggtitle("mTD")

# Combine plots side by side
combined_plot <- plot1 + plot2 +
  plot_layout(ncol = 2, byrow = TRUE)


#-------------FCS差值----------
plot1<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = positiveFCSd, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "FCS") +
  scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red", 
                       midpoint = 0, limits = c(-0.0058, 0.0035), oob = squish, na.value = "transparent") +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Positive")


plot2<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = negativeFCSd,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "FCS") +
  scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red", 
                       midpoint = 0, limits = c(-0.0058, 0.0035), oob = squish, na.value = "transparent") +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Negative")

# Combine plots side by side
combined_plot <- plot1 + plot2 +
  plot_layout(ncol = 2, byrow = TRUE)


#-------------trend----------
dataresult$TREND <- factor(dataresult$TREND)
# 绘图
ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = TREND, colour = I("black"), size = I(.08)), position = "stacked") +
  theme_minimal() +  
  labs(fill = "") +
  scale_fill_manual(values = c("1" = "#FFc0cB", "2" = "#ADD8E6","3" = "#F8B57B", "4" = "#1F77B4",  "NA" = "#CCCCCC"),
                    name = "Trend",
                    labels = c("上升","下降", "先降再升","先升再降", "NA")) +  
  guides(size = "none") +
  guides(fill = guide_legend(title = "Trend",
                             labels = c("上升","下降", "先降再升","先升再降","NA")))

dataresult$TYPE_p <- factor(dataresult$TYPE_p)
# 绘图
ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = TYPE_p, colour = I("black"), size = I(.08)), position = "stacked") +
  theme_minimal() +  
  labs(fill = "") +
  scale_fill_manual(values = c("1" = "#FFc0cB", "4" = "#F8B57B", "3" = "#1F77B4", "2" = "#ADD8E6", "NA" = "#CCCCCC"),
                    name = "Trend",
                    labels = c("上升","下降", "先降再升","先升再降", "NA")) +  
  guides(size = "none") +
  guides(fill = guide_legend(title = "Trend",
                             labels = c("上升","下降", "先降再升","先升再降","NA")))

dataresult$TYPE_p <- factor(dataresult$TYPE_p)
# 绘图
ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = TYPE_p, colour = I("black"), size = I(.08)), position = "stacked") +
  theme_minimal() +  
  labs(fill = "") +
  scale_fill_manual(values = c("1" = "#FFc0cB", "4" = "#F8B57B", "3" = "#1F77B4", "2" = "#ADD8E6", "NA" = "#E0E0E0"),
                    name = "Trend",
                    labels = c("上升","下降", "先降再升","先升再降", "NA")) +  
  guides(size = "none") +
  guides(fill = guide_legend(title = "Trend",
                             labels = c("上升","下降","先降再升","先升再降","NA")))

#-------------聚类值----------
plot1<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = cluster1, colour = I("black"), size = I(.08)), position = "stacked") +
  theme_void() +
  labs(fill = "Center") +
  scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red", 
                       midpoint = 0, limits = c(-0.5, 0.5), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Cluster1")


plot2<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = cluster2,colour=I("black"), size=I(.08)), position = "stacked") +
  theme_void() +
  labs(fill = "Center") +
  scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red", 
                       midpoint = 0, limits = c(-0.5, 0.5), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Cluster2")

# Combine plots side by side
combined_plot <- plot1 + plot2 +
  plot_layout(ncol = 2, byrow = TRUE)

#-------------贡献----------
# 2. 排序数据，根据Contribution列，选择最小的前20个ROI
sorted_data <- dataresult %>%
  arrange(Contribution) %>%
  head(20)  # 选择前20个最小的

# 3. 绘制条形图
ggplot(sorted_data, aes(x = reorder(ROI.Name, Contribution), y = Contribution)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # 绘制条形图
  labs(x = "ROI Name", y = "ANOVA P-Value", title = "Top 20 ROIs with Smallest ANOVA P-Value") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  # 旋转x轴标签，使其更清晰

plot1<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = cluster1, colour = I("black"), size = I(.08)), position = "stacked") +
  theme_void() +
  labs(fill = "Center") +
  scale_fill_gradient2(low = "blue", mid = "lightgrey", high = "red", 
                       midpoint = 0, limits = c(-0.5, 0.5), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Cluster1")


ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = Contribution,colour=I("black"), size=I(.08)), position = "stacked") +
  theme_void() +
  labs(fill = "Cluster significant contributing ROI") +
  scale_fill_gradient2(low = "#F08080", mid = "#F08080", high = "white", 
                       midpoint = 0.025, limits = c(0, 0.05), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("")


###
plotTD <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = positive_ratios_TD,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "Ratio") +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter",  direction = -1, limits = c(0, .03), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Overlap map of TD")

plotASD <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = positive_ratios_ASD,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "Ratio") +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter",direction = -1, limits = c(0, .03), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Overlap map of ASD")

combined_plot <- plotTD + plotASD +
  plot_layout(ncol = 2, byrow = TRUE)

###
plotTD <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = negative_ratios_TD,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "Ratio") +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter", direction = -1, limits = c(0, .06), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Overlap map of TD")

plotASD <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = negative_ratios_ASD,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "Ratio") +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter",  direction = -1, limits = c(0, .06), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Overlap map of ASD")

combined_plot <- plotTD + plotASD +
  plot_layout(ncol = 2, byrow = TRUE)

###
plotTD <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = all.ratio_TD,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "Ratio") +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter", na.value="transparent", direction = -1, limits = c(0, .08), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("TD")

plotASD <- ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = all.ratio_ASD,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "Ratio") +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter", na.value="transparent", direction = -1, limits = c(0, .08), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("ASD")

combined_plot <- plotTD + plotASD +
  plot_layout(ncol = 2, byrow = TRUE)
ggsave("combined_plot.pdf", combined_plot, width = 16, height = 9, units = "in", dpi = 300)

pdf("combined_plot.pdf", width = 16, height = 9, onefile = TRUE)

# 专门为Mac + ggseg优化的保存设置
ggsave("combined_plot_mac_fixed.pdf",
       plot = combined_plot,
       device = cairo_pdf,      # 在Mac上使用cairo
       width = 22,              # 大幅增加宽度
       height = 11,             # 相应增加高度
       units = "in",
       limitsize = FALSE)       # 必须关闭尺寸限制
###
ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = negative_difference_ratio, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "ΔOverlap") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-0.05, 0.05), oob = squish) +
  theme(legend.position = "bottom",legend.text =  element_text(family = "Arial", color = "black")) +
  ggtitle("ΔOverlap")

ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = positive_difference_ratio, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "ΔOverlap") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-0.05, 0.05), oob = squish) +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black")) +
  ggtitle("ΔOverlap")


library(dplyr)

#-------------极端偏差----------

# theme(legend.text = element_text(family = "Arial", color = "black"))
# 
# ggseg(.data = data0, atlas = schaefer17_200, mapping = aes(fill = np_values,colour=I("black"), size=I(.03)), position = "stacked") +
#   theme_void() +
#   labs(fill = "") +
#   paletteer::scale_fill_paletteer_c("pals::ocean.matter", na.value="transparent", direction = -1, limits = c(0, 0.05), oob = squish) +
#   theme(legend.text = element_text(family = "Arial", color = "black"))
#theme(legend.position = "none")


#极端偏差
plotn<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = fdr_n_m,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "") +
  scale_fill_gradient2(low = "blue",mid="#FF9999",high = "#FF9999", limits = c(-0.05, .0002), na.value = "transparent", oob = squish) +
  theme(legend.position = "none")+
  annotate("text", x = Inf, y = -50, label = "Negative extreme deviation area(P<0.05)", hjust = 1, vjust = 0, size = 4, color = "black", family = "Arial")

#极端偏差
plotp<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = fdr_p_m,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "") +
  scale_fill_gradient2(low = "blue",mid="#FF9999",high = "#FF9999", limits = c(-0.05, .0002), na.value = "transparent", oob = squish) +
  theme(legend.position = "none")+
  annotate("text", x = Inf, y = -50, label = "Positive extreme deviation area(P<0.05)", hjust = 1, vjust = 0, size = 4, color = "black", family = "Arial")

combined_plot <- plotp + plotn +
  plot_layout(ncol = 2, byrow = TRUE)







# 合并两列数据
combined_data <- data.frame(value = c(dataresult$negative_ratios_ASD, dataresult$negative_ratios_TD),
                            type = c(rep("ASD", nrow(dataresult)), rep("TD", nrow(dataresult))))
# 计算平均值和四分位数
mean_ASD <- mean(dataresult$negative_ratios_ASD)
q1_ASD <- quantile(dataresult$negative_ratios_ASD, probs = 0.25)
q3_ASD <- quantile(dataresult$negative_ratios_ASD, probs = 0.75)

mean_TD <- mean(dataresult$negative_ratios_TD)
q1_TD <- quantile(dataresult$negative_ratios_TD, probs = 0.25)
q3_TD <- quantile(dataresult$negative_ratios_TD, probs = 0.75)

# 绘制密度曲线
ggplot(combined_data, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean_ASD, linetype = "dashed", color = "red") +  # 添加ASD平均线
  geom_vline(xintercept = mean_TD, linetype = "dashed", color = "skyblue") +  # 添加TD平均线
  geom_vline(xintercept = q1_ASD,linetype = "dashed", color = "red") +  # 添加ASD
  geom_vline(xintercept = q3_ASD,linetype = "dashed", color = "red") +  # 添加ASD
  geom_vline(xintercept = q1_TD,linetype = "dashed", color = "skyblue") +  # 添加ASD
  geom_vline(xintercept = q3_TD,linetype = "dashed", color = "skyblue") +  # 添加TD平均线
  #geom_text(x = Inf, y = Inf, label = "P = 0.1436", color = "black", size = 4, vjust = -1) +  # 添加P值标签
  annotate("text", x = Inf, y = 60, label = "P = 1.6e-10**", hjust = 1, vjust = 0, size = 4, color = "black", family = "Arial")+
  labs(title = "Distribution Plot of Extreme Negative Deviation Proportions",
       x = "Negative Ratio",
       y = "Density") +
  scale_fill_manual(values = c("TD" = "skyblue", "ASD" = "red")) +
  theme_minimal()





# 合并两列数据
combined_data <- data.frame(value = c(dataresult$positive_ratios_ASD, dataresult$positive_ratios_TD),
                            
                            type = c(rep("ASD", nrow(dataresult)), rep("TD", nrow(dataresult))))
# 计算平均值和四分位数
mean_ASD <- mean(dataresult$positive_ratios_ASD)
q1_ASD <- quantile(dataresult$positive_ratios_ASD, probs = 0.25)
q3_ASD <- quantile(dataresult$positive_ratios_ASD, probs = 0.75)

mean_TD <- mean(dataresult$positive_ratios_TD)
q1_TD <- quantile(dataresult$positive_ratios_TD, probs = 0.25)
q3_TD <- quantile(dataresult$positive_ratios_TD, probs = 0.75)

# 绘制密度曲线
ggplot(combined_data, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mean_ASD, linetype = "dashed", color = "red") +  # 添加ASD平均线
  geom_vline(xintercept = mean_TD, linetype = "dashed", color = "skyblue") +  # 添加TD平均线
  geom_vline(xintercept = q1_ASD,linetype = "dashed", color = "red") +  # 添加ASD
  geom_vline(xintercept = q3_ASD,linetype = "dashed", color = "red") +  # 添加ASD
  geom_vline(xintercept = q1_TD,linetype = "dashed", color = "skyblue") +  # 添加ASD
  geom_vline(xintercept = q3_TD,linetype = "dashed", color = "skyblue") +  # 添加TD平均线
  #geom_text(x = Inf, y = Inf, label = "P = 0.1436", color = "black", size = 4, vjust = -1) +  # 添加P值标签
  annotate("text", x = Inf, y = 60, label = "P =4.1e-8***", hjust = 1, vjust = 0, size = 4, color = "black", family = "Arial")+
  labs(title = "Distribution Plot of Extreme Positive Deviation Proportions",
       x = "Positive Ratio",
       y = "Density") +
  scale_fill_manual(values = c("ASD" = "red", "TD" = "skyblue")) +
  theme_minimal()


#correlation
setwd("E:/bishe/modelwc/correlation")
# 安装并加载必要的包
# 导入数据
correlation <- read_excel(".xlsx")

#极端偏差
ggseg(.data = correlation, atlas = schaefer17_200, mapping = aes(fill = Count, colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "") +
  scale_fill_gradient2(low = "white",  high = "red", 
                       midpoint = 0, limits = c(0, 7), oob = squish) +
  theme(legend.text = element_text(family = "Arial", color = "black"))

setwd("E:/bishe/modelwc")
dataregion <- read_excel("regioncheck.xlsx")
dataregion$typee <- factor(dataregion$typee)
ggseg(.data = dataregion, atlas = schaefer17_200, mapping = aes(fill = typee, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "") +
  scale_fill_manual(values = c("1" = "#FFC0CB","2"="red","3"="blue","4"="green","5"="black","6"="yellow"),
                    name = "Trend")

ggseg(.data = dataregion, atlas = schaefer17_200, mapping = aes(fill = typee,colour=I("black"), size=I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "") +
  paletteer::scale_fill_paletteer_c("pals::ocean.matter", na.value="transparent", direction = -1, limits = c(1, 7), oob = squish) +
  theme(legend.text = element_text(family = "Arial", color = "black"))


setwd("E:/bishe/modelwc/selected results")
data <- read.csv("GMM_mu.csv")
ggseg(.data = data, atlas = schaefer17_200, mapping = aes(fill = X5, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "μ") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-1.19, 1.76), oob = squish) +
  theme(legend.position = "bottom", legend.title = element_text(family = "Arial", color = "black"), legend.text = element_text(family = "Arial", color = "black")) +
  ggtitle("Subtype5")


plot1<-ggseg(.data = dataresult, atlas = schaefer17_200, mapping = aes(fill = k2_c, colour = I("black"), size = I(.03)), position = "stacked") +
  theme_void() +
  labs(fill = "Z score") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                        limits = c(-0.71, 0.98), oob = squish, na.value = "lightgrey") +
  theme(legend.position = "bottom",legend.text = element_text(family = "Arial", color = "black"))+
  ggtitle("Center")