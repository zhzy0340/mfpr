#vegan包可以用来标准化和中心化数据
library(cowplot) 
library(devtools)
library(pheatmap) # 本次所使用的热图绘制包
library(vegan)  #vegan包可以用来标准化和中心化数据
setwd(dir="/Users/yunya/Documents/0.35fd/asd/17")
r_matrix <- read.csv("/Users/yunya/Documents/0.35fd/asd/r_matrix.csv", row.names = 1)
p_matrix <- read.csv("/Users/yunya/Documents/0.35fd/asd/p_matrix.csv", row.names = 1)

# 读取左半球数据并绘制热图
data_left <- read.table("input.txt", header = T, sep = "\t", row.names = 1)
data_left <- as.data.frame(data_left) 
p1 <- pheatmap(data_left, cluster_rows = FALSE, cluster_cols = FALSE, 
               fontsize = 2, cellwidth = 6, cellheight = 2)

# 读取右半球数据并绘制热图
data_right <- read.table("input-r.txt", header = T, sep = "\t", row.names = 1)
data_right <- as.data.frame(data_right) 
p2 <- pheatmap(data_right, cluster_rows = FALSE, cluster_cols = FALSE, 
               fontsize = 2, cellwidth = 6, cellheight = 2)

# 使用 plot_grid() 拼接两个热图
combined_plot <- plot_grid(p1$gtable, p2$gtable, ncol = 2)  # 横向拼接

dev.off()


datad<-read.table("default.txt",header=T,sep="\t",row.names=1)
datad <- data.frame(datad) 
pheatmap(datad, cluster_rows = F,cluster_cols = F,fontsize = 2,cellwidth=6,cellheight=2)




data1<-read.table("p-l.txt",header=T,sep="\t",row.names=1)
data1 <- data.frame(data1) 
data2<-read.table("p.txt",header=T,sep="\t",row.names=1)
data2 <- data.frame(data2)
# 设置红色色卡
red_palette <- colorRampPalette(c("red", "white"))(256)
# 确保所有列都是数值型，否则转换它们  
data1_numeric <- data1  
data1_numeric[sapply(data1, is.factor)] <- lapply(data1[sapply(data1, is.factor)], as.numeric)  
data1_numeric[sapply(data1_numeric, is.character)] <- lapply(data1_numeric[sapply(data1_numeric, is.character)], as.numeric)  
data2_numeric <- data2  
data2_numeric[sapply(data2, is.factor)] <- lapply(data2[sapply(data2, is.factor)], as.numeric)  
data2_numeric[sapply(data2_numeric, is.character)] <- lapply(data2_numeric[sapply(data2_numeric, is.character)], as.numeric)  

# 右半球热图
rh <- pheatmap(data1_numeric,   
               cluster_rows = FALSE,   
               cluster_cols = FALSE,   
               fontsize = 2,   
               cellwidth = 6,   
               cellheight = 2,   
               legend_height = 0.1,  
               color = red_palette,  
               na_color = "white",   
               main = "Right Hemisphere")  # 添加标题

# 左半球热图
lh <- pheatmap(data2_numeric,   
               cluster_rows = FALSE,   
               cluster_cols = FALSE,   
               fontsize = 2,   
               cellwidth = 6,   
               cellheight = 2,   
               legend_height = 0.1,  
               color = red_palette,  
               na_color = "white",   
               #main = "Left Hemisphere"
               )  # 添加标题

# 使用 plot_grid() 将两个热图拼接在一起
plot_grid(lh$gtable, rh$gtable, ncol = 2)  # 设置 ncol = 2 表示横向拼接

