  #vegan包可以用来标准化和中心化数据

library(devtools)
library(pheatmap) # 本次所使用的热图绘制包
library(vegan)  #vegan包可以用来标准化和中心化数据
setwd(dir="/Users/yunya/Documents/reho/reho2/pearson")
#setwd("E:/bishe/modelwc/correlation")
#导入数据
data<-read.table("input_r.txt",header=T,sep="\t",row.names=1)
data <- as.data.frame(data) 
p1<-pheatmap(data,cluster_rows = F,cluster_cols = F,fontsize = 2,cellwidth=6,cellheight=2)
# 开始PDF图形设备，并设置文件名和分辨率  
pdf("correlation_heatmap.pdf", width=8, height=6) # 设置分辨率为300 DPI  
dev.off()

# 生成热图  
pheatmap(data, cluster_rows = FALSE, cluster_cols = FALSE, fontsize = 2, cellwidth = 6, cellheight = 2)  

datad<-read.table("default.txt",header=T,sep="\t",row.names=1)
datad <- data.frame(datad) 
pheatmap(datad, cluster_rows = F,cluster_cols = F,fontsize = 2,cellwidth=6,cellheight=2)


data1<-read.table("RO.txt",header=T,sep="\t",row.names=1)
data1 <- data.frame(data1) 
pheatmap(data1,cluster_rows = F,cluster_cols = F,fontsize = 2,cellwidth=6,cellheight=2)


# 设置红色色卡
red_palette <- colorRampPalette(c("red", "white"))(256)
# 确保所有列都是数值型，否则转换它们  
data1_numeric <- data1  
data1_numeric[sapply(data1, is.factor)] <- lapply(data1[sapply(data1, is.factor)], as.numeric)  
data1_numeric[sapply(data1_numeric, is.character)] <- lapply(data1_numeric[sapply(data1_numeric, is.character)], as.numeric)  

# 绘制热图，将NA值设为白色  
lh <- pheatmap(data1_numeric,   
               cluster_rows = FALSE,   
               cluster_cols = FALSE,   
               fontsize = 2,   
               cellwidth = 6,   
               cellheight = 2,   
               legend_height = 0.1,  
               color = red_palette,  
               na_color = "white")



library(devtools)
library(patchwork)
rh+lh+plot_layout(nrow=1)

library(pheatmap)

# 假设数据框已经加载到 data 中

# 创建一个颜色渐变从红色到白色，颜色更柔和
p1 <- pheatmap(data, 
               cluster_rows = F, 
               cluster_cols = F, 
               fontsize = 2, 
               cellwidth = 6, 
               cellheight = 2,
               color = colorRampPalette(c("black",  "white"))(100))  # 使用深红色到浅粉色到白色的渐变

library(pheatmap)


# 假设数据框已经加载到 data 中

# 创建热图
p1 <- pheatmap(data, 
               cluster_rows = F, 
               cluster_cols = F, 
               fontsize = 2, 
               cellwidth = 6, 
               cellheight = 2,
               color = colorRampPalette(c("red", "white"))(100))  # 红色到白色的渐变色