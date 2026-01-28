# mfpr
代码说明：
  主要的mfp实现（十折交叉）：jiaochao.r。  
  根据常模计算偏离z分数：zscore.r。  
  为了确保可靠性，将十次常模得到的z分数平均，并计算极端偏离的比例：zscoreave.r。  
  极端偏离比例经过置换检验看哪些脑区有显著差异：zhihuan.r。  
  将比例进行小提琴图可视化：violin.r/分组小提琴图。  
  用z分数进行聚类，将asd聚成不同亚类：k.r。  
  每个亚类进行行为学和z分数的相关：pearsonf.r。 进行热力图可视化：relitu.r。  
  预测行为学指标效果的对比：predict-compare.r。  
  结果图：modelplot.r brainplot.r  

