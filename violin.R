library(ggplot2)
library(ggsignif)

setwd(dir="/Users/yunya/Documents/0.35FD/")
dataresult<-read_excel("modelresult.xlsx")
data<-read_excel("score.xlsx")
# Perform paired t-test
t_test_result <- t.test(dataresult$mASD_FCS, dataresult$mTD_FCS, paired = FALSE)

# Create violin plots for mASD_FCS and mTD_FCS
gene1_violin <- ggplot(dataresult, aes(x = factor(1), y = mTD_FCS, fill = "mTD_FCS")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_jitter(aes(x = factor(1), y = mTD_FCS, color = "mTD_FCS"), width = 0.1, alpha = 0.5) +  # Add jittered points for visibility
  geom_violin(aes(x = factor(2), y = mASD_FCS, fill = "mASD_FCS"), trim = FALSE, color = "white") +
  geom_jitter(aes(x = factor(2), y = mASD_FCS, color = "mASD_FCS"), width = 0.1, alpha = 0.5) +  # Add jittered points for visibility
  geom_boxplot(aes(x = factor(1), y = mTD_FCS, fill = "mTD_FCS"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mASD_FCS
  geom_boxplot(aes(x = factor(2), y = mASD_FCS, fill = "mASD_FCS"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mTD_FCS
  scale_color_manual(values = c("mTD_FCS" = "#1F77b4", "mASD_FCS" = "#FF9999")) + # Set colors manually
  scale_fill_manual(values = c("mTD_FCS" = "#1F77b4", "mASD_FCS" = "#FF9999")) + # Set colors manually
  scale_x_discrete(labels = c("TD", "ASD")) + # Change x-axis labels
  labs(x = "", y = "FCS") + # Axes labels
  theme_bw() + 
  theme(legend.position = "top", # Set legend position
        axis.text.x = element_text(colour = "black", family = "Times", size = 15), # x-axis font
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"), # y-axis font
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"), # x-axis label font
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"), # y-axis label font
        panel.border = element_blank(), # Remove panel borders
        axis.line = element_line(colour = "black", size = 1), # Set axis line color and size
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        title = element_text(family = "Times", size = 14, face = "plain") # Set title font
  ) +
  labs(fill = "") + # Remove fill legend title
  geom_signif(comparisons = list(c("TD", "ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +  # Significance labels
  annotate("text", x = 1.5, y = -Inf, label = paste("t =", round(t_test_result$statistic, 2), ", p =", signif(t_test_result$p.value, digits = 2)), hjust = 0.5, vjust = -0.5, size = 4)  # Add t-test results
  
# Combine the plots
combined_plot <- gene39_violin

# Perform paired t-test
t_test_result <- t.test(dataresult$positive_ratio_TD, dataresult$positive_ratio_ASD, paired = FALSE)

gene2_violin <- ggplot(dataresult, aes(x = factor(1), y = positive.ratio_TD, fill = "positive.ratio_TD")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_point(aes(x = factor(1), y = positive.ratio_TD, color = "positive.ratio_TD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
  geom_violin(aes(x = factor(2), y = positive.ratio_ASD, fill = "positive.ratio_ASD"), trim = FALSE, color = "white") +
  geom_point(aes(x = factor(2), y = positive.ratio_ASD, color = "positive.ratio_ASD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
  geom_boxplot(aes(x = factor(1), y = positive.ratio_TD, fill = "positive.ratio_TD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mASD_FCS
  geom_boxplot(aes(x = factor(2), y = positive.ratio_ASD, fill = "positive.ratio_ASD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mTD_FCS
  scale_color_manual(values = c("positive.ratio_TD" = "#1F77b4", "positive.ratio_ASD" = "#FF9999")) + # Set colors manually
  scale_fill_manual(values = c("positive.ratio_TD" = "#1F77b4", "positive.ratio_ASD" = "#FF9999")) + # Set colors manually
  scale_x_discrete(labels = c("Overlap_TD", "Overlap_ASD")) + # Change x-axis labels
  labs(x = "", y = "Proportion") + # Axes labels
  theme_bw() + 
  theme(legend.position = "top", # Set legend position
        axis.text.x = element_text(colour = "black", family = "Times", size = 15), # x-axis font
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"), # y-axis font
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"), # x-axis label font
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"), # y-axis label font
        panel.border = element_blank(), # Remove panel borders
        axis.line = element_line(colour = "black", size = 1), # Set axis line color and size
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        title = element_text(family = "Times", size = 14, face = "plain") # Set title font
  ) +
  labs(fill = "") + # Remove fill legend title
  geom_signif(comparisons = list(c("Overlap_TD", "Overlap_ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +  # Significance labels
  annotate("text", x = 1.5, y = -Inf, label = paste("t =", round(t_test_result$statistic, 2), ", p =", signif(t_test_result$p.value, digits = 2),"***"), hjust = 0.5, vjust = -0.5, size = 4)  # Add t-test results

# Combine the plots
combined_plot <- gene2_violin

# Perform paired t-test
t_test_result <- t.test(dataresult$negative.ratio_TD, dataresult$negative_ratioASD, paired = FALSE)

gene3_violin <- ggplot(dataresult, aes(x = factor(1), y = negative.ratio_TD, fill = "negative.ratio_TD")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_point(aes(x = factor(1), y = negative.ratio_TD, color = "negative.ratio_TD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
  geom_violin(aes(x = factor(2), y = positive.ratio_ASD, fill = "negative_ratioASD"), trim = FALSE, color = "white") +
  geom_point(aes(x = factor(2), y = positive.ratio_ASD, color = "negative_ratioASD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
  geom_boxplot(aes(x = factor(1), y = negative.ratio_TD, fill = "negative.ratio_TD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mASD_FCS
  geom_boxplot(aes(x = factor(2), y = positive.ratio_ASD, fill = "negative_ratioASD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mTD_FCS
  scale_color_manual(values = c("negative.ratio_TD" = "#1F77b4", "negative_ratioASD" = "#FF9999")) + # Set colors manually
  scale_fill_manual(values = c("negative.ratio_TD" = "#1F77b4", "negative_ratioASD" = "#FF9999")) + # Set colors manually
  scale_x_discrete(labels = c("Overlap_TD", "Overlap_ASD")) + # Change x-axis labels
  labs(x = "", y = "Proportion") + # Axes labels
  theme_bw() + 
  theme(legend.position = "top", # Set legend position
        axis.text.x = element_text(colour = "black", family = "Times", size = 15), # x-axis font
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"), # y-axis font
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"), # x-axis label font
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"), # y-axis label font
        panel.border = element_blank(), # Remove panel borders
        axis.line = element_line(colour = "black", size = 1), # Set axis line color and size
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        title = element_text(family = "Times", size = 14, face = "plain") # Set title font
  ) +
  labs(fill = "") + # Remove fill legend title
  geom_signif(comparisons = list(c("Overlap_TD", "Overlap_ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +  # Significance labels
  annotate("text", x = 1.5, y = -Inf, label = paste("t =", round(t_test_result$statistic, 2), ", p =", signif(t_test_result$p.value, digits = 2),"***"), hjust = 0.5, vjust = -0.5, size = 4)  # Add t-test results

# Combine the plots
combined_plot <- gene3_violin

# Perform Mann-Whitney U test
u_test_result <- wilcox.test(dataresult$negative_ratios_TD, dataresult$negative_ratios_ASD)
#u_test_result <-  t.test(dataresult$negative_ratios_TD, dataresult$negative_ratios_ASD)

gene3_violin <- ggplot(dataresult, aes(x = factor(1), y = negative_ratios_TD, fill = "negative_ratios_TD")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_violin(aes(x = factor(2), y = negative_ratios_ASD, fill = "negative_ratios_ASD"), trim = FALSE, color = "white") +
  geom_boxplot(aes(x = factor(1), y = negative_ratios_TD, fill = "negative_ratios_TD"), width = 0.2, position = position_dodge(0.9)) +
  geom_boxplot(aes(x = factor(2), y = negative_ratios_ASD, fill = "negative_ratios_ASD"), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("negative_ratios_TD" = "#1F77b4", "negative_ratios_ASD" = "#FF9999")) +
  scale_x_discrete(labels = c("Overlap_TD", "Overlap_ASD")) +
  labs(x = "", y = "Proportion") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(colour = "black", family = "Times", size = 15),
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"),
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"),
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(family = "Times", size = 14, face = "plain")
  ) +
  labs(fill = "") +
  geom_signif(comparisons = list(c("Overlap_TD", "Overlap_ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +
  annotate("text", 
           x = 1.5, 
           y = -Inf, 
           label = paste("U =", round(u_test_result$statistic, 2), 
                         ", p =", format(u_test_result$p.value, digits = 2, nsmall = 2), 
                         "***"), 
           hjust = 0.5, 
           vjust = -0.5, 
           size = 4)
# Combine the plots
combined_plot <- gene3_violin

# Perform Mann-Whitney U test
u_test_result <- wilcox.test(dataresult$positive_ratios_TD, dataresult$positive_ratios_ASD)
#u_test_result <- t.test(dataresult$positive_ratios_TD, dataresult$positive_ratios_ASD)
gene4_violin <- ggplot(dataresult, aes(x = factor(1), y = positive_ratios_TD, fill = "positive_ratios_TD")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_violin(aes(x = factor(2), y = positive_ratios_ASD, fill = "positive_ratios_ASD"), trim = FALSE, color = "white") +
  geom_boxplot(aes(x = factor(1), y = positive_ratios_TD, fill = "positive_ratios_TD"), width = 0.2, position = position_dodge(0.9)) +
  geom_boxplot(aes(x = factor(2), y = positive_ratios_ASD, fill = "positive_ratios_ASD"), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("positive_ratios_TD" = "#1F77b4", "positive_ratios_ASD" = "#FF9999")) +
  scale_x_discrete(labels = c("Overlap_TD", "Overlap_ASD")) +
  labs(x = "", y = "Proportion") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(colour = "black", family = "Times", size = 15),
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"),
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"),
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(family = "Times", size = 14, face = "plain")
  ) +
  labs(fill = "") +
  geom_signif(comparisons = list(c("Overlap_TD", "Overlap_ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +
  annotate("text", x = 1.5, y = -Inf, label = paste("T =", u_test_result$statistic, ", p =", signif(u_test_result$p.value, digits = 2), "***"), hjust = 0.5, vjust = -0.5, size = 4)

# Combine the plots
combined_plot <- gene4_violin
# # Perform paired t-test
# u_test_result <- wilcox.test(dataresult$positive.ratio_TD, dataresult$positive.ratio_ASD)
# 
# gene4_violin <- ggplot(dataresult, aes(x = factor(1), y = positive.ratio_TD, fill = "positive.ratio_TD")) +
#   geom_violin(trim = FALSE, color = "white") +
#   geom_point(aes(x = factor(1), y = positive.ratio_TD, color = "positive.ratio_TD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
#   geom_violin(aes(x = factor(2), y = positive.ratio_ASD, fill = "positive.ratio_ASD"), trim = FALSE, color = "white") +
#   geom_point(aes(x = factor(2), y = positive.ratio_ASD, color = "positive.ratio_ASD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
#   geom_boxplot(aes(x = factor(1), y = positive.ratio_TD, fill = "positive.ratio_TD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mASD_FCS
#   geom_boxplot(aes(x = factor(2), y = positive.ratio_ASD, fill = "positive.ratio_ASD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mTD_FCS
#   scale_color_manual(values = c("positive.ratio_TD" = "#1F77b4", "positive.ratio_ASD" = "#FF9999")) + # Set colors manually
#   scale_fill_manual(values = c("positive.ratio_TD" = "#1F77b4", "positive.ratio_ASD" = "#FF9999")) + # Set colors manually
#   scale_x_discrete(labels = c("Overlap_TD", "Overlap_ASD")) + # Change x-axis labels
#   labs(x = "", y = "Proportion") + # Axes labels
#   theme_bw() + 
#   theme(legend.position = "top", # Set legend position
#         axis.text.x = element_text(colour = "black", family = "Times", size = 15), # x-axis font
#         axis.text.y = element_text(family = "Times", size = 12, face = "plain"), # y-axis font
#         axis.title.x = element_text(family = "Times", size = 16, face = "plain"), # x-axis label font
#         axis.title.y = element_text(family = "Times", size = 16, face = "plain"), # y-axis label font
#         panel.border = element_blank(), # Remove panel borders
#         axis.line = element_line(colour = "black", size = 1), # Set axis line color and size
#         panel.grid.major = element_blank(), # Remove major grid lines
#         panel.grid.minor = element_blank(), # Remove minor grid lines
#         title = element_text(family = "Times", size = 14, face = "plain") # Set title font
#   ) +
#   labs(fill = "") + # Remove fill legend title
#   geom_signif(comparisons = list(c("Overlap_TD", "Overlap_ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +  # Significance labels
#   annotate("text", x = 1.5, y = -Inf, label = paste("U =", round(u_test_result$statistic, 2), ", p =", signif(u_test_result$p.value, digits = 2),"***"), hjust = 0.5, vjust = -0.5, size = 4)  # Add t-test results
# 
# # Combine the plots
# combined_plot <- gene4_violin



# Perform paired t-test
u_test_result <- wilcox.test(dataresult$positive_ratios_TD, dataresult$positive_ratios_ASD)

gene4_violin <- ggplot(dataresult, aes(x = factor(1), y = positive_ratios_TD, fill = "positive_ratios_TD")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_point(aes(x = factor(1), y = positive_ratios_TD, color = "positive_ratios_TD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
  geom_violin(aes(x = factor(2), y = positive_ratios_ASD, fill = "positive_ratios_ASD"), trim = FALSE, color = "white") +
  geom_point(aes(x = factor(2), y = positive_ratios_ASD, color = "positive_ratios_ASD"), position = "jitter", width = 0.1, alpha = 0) +  # Hide data points
  geom_boxplot(aes(x = factor(1), y = positive_ratios_TD, fill = "positive_ratios_TD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mASD_FCS
  geom_boxplot(aes(x = factor(2), y = positive_ratios_ASD, fill = "positive_ratios_ASD"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for mTD_FCS
  scale_color_manual(values = c("positive_ratios_TD" = "skyblue", "positive_ratios_ASD" = "#FF7F7F")) + # Set colors manually
  scale_fill_manual(values = c("positive_ratios_TD" = "skyblue", "positive_ratios_ASD" ="#FF7F7F")) + # Set colors manually
  scale_x_discrete(labels = c("Overlap_TD", "Overlap_ASD")) + # Change x-axis labels
  labs(x = "", y = "Proportion") + # Axes labels
  theme_bw() + 
  theme(legend.position = "top", # Set legend position
        axis.text.x = element_text(colour = "black", family = "Times", size = 15), # x-axis font
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"), # y-axis font
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"), # x-axis label font
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"), # y-axis label font
        panel.border = element_blank(), # Remove panel borders
        axis.line = element_line(colour = "black", size = 1), # Set axis line color and size
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        title = element_text(family = "Times", size = 14, face = "plain") # Set title font
  ) +
  labs(fill = "") + # Remove fill legend title
  geom_signif(comparisons = list(c("Overlap_TD", "Overlap_ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +  # Significance labels
  annotate("text", x = 1.5, y = -Inf, label = paste("U =", round(u_test_result$statistic, 2), ", p =", signif(u_test_result$p.value, digits = 2),"***"), hjust = 0.5, vjust = -0.5, size = 4)  # Add t-test results



# Perform Mann-Whitney U test
u_test_result <- wilcox.test(dataresult$negative.ratio_TD, dataresult$negative.ratio_ASD)

gene3_violin <- ggplot(dataresult, aes(x = factor(1), y = negative.ratio_TD, fill = "negative.ratio_TD")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_violin(aes(x = factor(2), y = negative.ratio_ASD, fill = "negative.ratio_ASD"), trim = FALSE, color = "white") +
  geom_boxplot(aes(x = factor(1), y = negative.ratio_TD, fill = "negative.ratio_TD"), width = 0.2, position = position_dodge(0.9)) +
  geom_boxplot(aes(x = factor(2), y = negative.ratio_ASD, fill = "negative.ratio_ASD"), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("negative.ratio_TD" = "skyblue", "negative.ratio_ASD" = "#FF7F7F")) +
  scale_x_discrete(labels = c("Overlap_TD", "Overlap_ASD")) +
  labs(x = "", y = "Proportion") +
  theme_bw() +
  theme(legend.position = "top",
        axis.text.x = element_text(colour = "black", family = "Times", size = 15),
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"),
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"),
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black", size = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_text(family = "Times", size = 14, face = "plain")
  ) +
  labs(fill = "") +
  geom_signif(comparisons = list(c("Overlap_TD", "Overlap_ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +
  annotate("text", x = 1.5, y = -Inf, label = paste("U =", u_test_result$statistic, ", p =", signif(u_test_result$p.value, digits = 2), "***"), hjust = 0.5, vjust = -0.5, size = 4)


setwd("E:/bishe/modelwc/selected results")
kmeans1 <- read_excel("k-means1.xlsx")
kmeans2 <- read_xls("k-means2.xls")
# Perform Mann-Whitney U test
u_test_result1 <- wilcox.test(kmeans1$AGE, kmeans2$AGE)
u_test_result2 <- wilcox.test(kmeans1$ADI_R_SOCIAL_TOTAL_A, kmeans2$ADI_R_SOCIAL_TOTAL_A)
# 假设我们有一个数据框架，它包含两个数据集kmeans1和kmeans2的AGE列  
# 我们可以使用rbind或其他方法将这两个数据集合并到一个数据框架中，并添加一个标识列来区分它们  
#combined_data <- rbind(data.frame(AGE = kmeans1$AGE, group = "kmeans1"), data.frame(AGE = kmeans2$AGE, group = "kmeans2"))  
combined_data <- rbind(data.frame(ADI_R_SOCIAL_TOTAL_A = kmeans1$ADI_R_SOCIAL_TOTAL_A, group = "Subtype 1"), data.frame(ADI_R_SOCIAL_TOTAL_A = kmeans2$ADI_R_SOCIAL_TOTAL_A, group = "Subtype 2"))  

# 绘制小提琴图  
gene3_violin <- ggplot(combined_data, aes(x = group, y = ADI_R_SOCIAL_TOTAL_A, fill = group)) +  
  geom_violin(trim = FALSE) +  
  geom_boxplot(width = 0.2) +  
  scale_fill_manual(values = c("Subtype 1" = "#FF7F7F", "Subtype 2" = "skyblue")) +  
  labs(x = "Cluster", y = "ADI_R_SOCIAL_TOTAL_A", fill = "Cluster") +  
  theme_bw() +  
  theme(legend.position = "top",  
        axis.text = element_text(family = "Times", size = 12),  
        axis.title = element_text(family = "Times", size = 14),  
        panel.border = element_blank(),  
        axis.line = element_line(colour = "black"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        title = element_text(family = "Times", size = 14, face = "plain")
  ) + 
  labs(fill = "") +
  geom_signif(comparisons = list(c("Subtype 1", "Subtype 2")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +
  annotate("text", x = 1.5, y = -Inf, label = paste("U =", u_test_result2$statistic, ", p = 0.068*"), hjust = 0.5, vjust = -0.5, size = 4)


# 显示图形  
print(gene3_violin)

combined_data <- rbind(data.frame(ADI_R_SOCIAL_TOTAL_A = kmeans1$ADI_R_SOCIAL_TOTAL_A, group = "Subtype 1"), data.frame(ADI_R_SOCIAL_TOTAL_A = kmeans2$ADI_R_SOCIAL_TOTAL_A, group = "Subtype 2"))  

# 绘制小提琴图  
gene3_violin <- ggplot(combined_data, aes(x = group, y = ADI_R_SOCIAL_TOTAL_A, fill = group)) +  
  geom_violin(trim = FALSE) +  
  geom_boxplot(width = 0.2) +  
  scale_fill_manual(values = c("Subtype 1" = "#FF7F7F", "Subtype 2" = "skyblue")) +  
  labs(x = "Cluster", y = "ADI_R_SOCIAL_TOTAL_A", fill = "Cluster") +  
  theme_bw() +  
  theme(legend.position = "top",  
        axis.text = element_text(family = "Times", size = 12),  
        axis.title = element_text(family = "Times", size = 14),  
        panel.border = element_blank(),  
        axis.line = element_line(colour = "black"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        title = element_text(family = "Times", size = 14, face = "plain")
  ) + 
  labs(fill = "") +
  geom_signif(comparisons = list(c("Subtype 1", "Subtype 2")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +
  annotate("text", x = 1.5, y = -Inf, label = paste("U =", u_test_result2$statistic, ", p = 0.068", signif(u_test_result1$p.value, digits = 2), "*"), hjust = 0.5, vjust = -0.5, size = 4)

#-------------zscore----------
# Perform paired t-test
t_test_result <- t.test(dataresult$z.asd, dataresult$z.td, paired = FALSE)

# Create violin plots for z.asd and z.td
gene1_violin <- ggplot(dataresult, aes(x = factor(1), y = z.td, fill = "z.td")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_jitter(aes(x = factor(1), y = z.td, color = "z.td"), width = 0.1, alpha = 0.5) +  # Add jittered points for visibility
  geom_violin(aes(x = factor(2), y = z.asd, fill = "z.asd"), trim = FALSE, color = "white") +
  geom_jitter(aes(x = factor(2), y = z.asd, color = "z.asd"), width = 0.1, alpha = 0.5) +  # Add jittered points for visibility
  geom_boxplot(aes(x = factor(1), y = z.td, fill = "z.td"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for z.asd
  geom_boxplot(aes(x = factor(2), y = z.asd, fill = "z.asd"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for z.td
  scale_color_manual(values = c("z.td" = "#1F77b4", "z.asd" = "#FF9999")) + # Set colors manually
  scale_fill_manual(values = c("z.td" = "#1F77b4", "z.asd" = "#FF9999")) + # Set colors manually
  scale_x_discrete(labels = c("TD", "ASD")) + # Change x-axis labels
  labs(x = "", y = "FCS") + # Axes labels
  theme_bw() + 
  theme(legend.position = "top", # Set legend position
        axis.text.x = element_text(colour = "black", family = "Times", size = 15), # x-axis font
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"), # y-axis font
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"), # x-axis label font
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"), # y-axis label font
        panel.border = element_blank(), # Remove panel borders
        axis.line = element_line(colour = "black", size = 1), # Set axis line color and size
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        title = element_text(family = "Times", size = 14, face = "plain") # Set title font
  ) +
  labs(fill = "") + # Remove fill legend title
  geom_signif(comparisons = list(c("TD", "ASD")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +  # Significance labels
  annotate("text", x = 1.5, y = -Inf, label = paste("t =", round(t_test_result$statistic, 2), ", p =", signif(t_test_result$p.value, digits = 2)), hjust = 0.5, vjust = -0.5, size = 4)  # Add t-test results

# Combine the plots
combined_plot <- gene1_violin



#-------------score----------
# Perform paired t-test
t_test_result <- t.test(data$ADOS_2_RRB_2, data$ADOS_2_RRB_1, paired = FALSE)

# Create violin plots for ADOS_2_RRB_2 and ADOS_2_RRB_1
gene1_violin <- ggplot(data, aes(x = factor(1), y = ADOS_2_RRB_1, fill = "ADOS_2_RRB_1")) +
  geom_violin(trim = FALSE, color = "white") +
  geom_jitter(aes(x = factor(1), y = ADOS_2_RRB_1, color = "ADOS_2_RRB_1"), width = 0.1, alpha = 0.5) +  # Add jittered points for visibility
  geom_violin(aes(x = factor(2), y = ADOS_2_RRB_2, fill = "ADOS_2_RRB_2"), trim = FALSE, color = "white") +
  geom_jitter(aes(x = factor(2), y = ADOS_2_RRB_2, color = "ADOS_2_RRB_2"), width = 0.1, alpha = 0.5) +  # Add jittered points for visibility
  geom_boxplot(aes(x = factor(1), y = ADOS_2_RRB_1, fill = "ADOS_2_RRB_1"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for ADOS_2_RRB_2
  geom_boxplot(aes(x = factor(2), y = ADOS_2_RRB_2, fill = "ADOS_2_RRB_2"), width = 0.2, position = position_dodge(0.9)) + # Add boxplot for ADOS_2_RRB_1
  scale_color_manual(values = c("ADOS_2_RRB_1" = "#1F77b4", "ADOS_2_RRB_2" = "#FF9999")) + # Set colors manually
  scale_fill_manual(values = c("ADOS_2_RRB_1" = "#1F77b4", "ADOS_2_RRB_2" = "#FF9999")) + # Set colors manually
  scale_x_discrete(labels = c("Cluster 1", "Cluster2")) + # Change x-axis labels
  labs(x = "", y = "ADOS_2_RRB") + # Axes labels
  theme_bw() + 
  theme(legend.position = "top", # Set legend position
        axis.text.x = element_text(colour = "black", family = "Times", size = 15), # x-axis font
        axis.text.y = element_text(family = "Times", size = 12, face = "plain"), # y-axis font
        axis.title.x = element_text(family = "Times", size = 16, face = "plain"), # x-axis label font
        axis.title.y = element_text(family = "Times", size = 16, face = "plain"), # y-axis label font
        panel.border = element_blank(), # Remove panel borders
        axis.line = element_line(colour = "black", size = 1), # Set axis line color and size
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        title = element_text(family = "Times", size = 14, face = "plain") # Set title font
  ) +
  labs(fill = "") + # Remove fill legend title
  geom_signif(comparisons = list(c("Cluster 1", "Cluster2")), map_signif_level = TRUE, textsize = 4, vjust = 0.5) +  # Significance labels
  annotate("text", x = 1.5, y = -Inf, label = paste("t =", round(t_test_result$statistic, 2), ", p =", signif(t_test_result$p.value, digits = 2)), hjust = 0.5, vjust = -0.5, size = 4)  # Add t-test results

# Combine the plots
combined_plot <- gene1_violin