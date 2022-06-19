library(tidyverse)
library(rmarkdown)
library(Seurat)
library(plotly)
library(tsne)
library(data.table)

# 读取PBMC数据集
pbmc.data <- Read10X(data.dir = "../filtered_feature_bc_matrix/")
# 使用原始数据（未归一化处理）初始化Seurat对象
pbmc <- CreateSeuratObject(counts = pbmc.data, project = "pbmc5k", min.cells = 3, min.features = 200)
pbmc

#读入数据
## check at metadata
head(pbmc@meta.data)
# The [[ operator can add columns to object metadata. This is a great place to stash QC stats (线粒体含量)
pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc, pattern = "^MT-")
pbmc@meta.data %>% head()
##将质量控制指标可视化为小提琴图
VlnPlot(pbmc, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)
dim(pbmc.data)
pbmc <- subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 5000 & percent.mt < 25)
dim(pbmc.data)

#Normalization
##通常情况下，我们采用全局缩放的归一化方法"LogNormalize"
pbmc <- NormalizeData(pbmc, normalization.method = "LogNormalize", scale.factor = 10000)

#特征选择
pbmc <- FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)

# Identify the 10 most highly variable genes
top10 <- head(VariableFeatures(pbmc), 10)

# plot variable features with and without labels
plot1 <- VariableFeaturePlot(pbmc)
plot2 <- LabelPoints(plot = plot1, points = top10, repel = TRUE)

CombinePlots(plots = list(plot1, plot2), ncol =1)

#Scaling the data
all.genes <- rownames(pbmc)
pbmc <- ScaleData(pbmc, features = all.genes)

#在scale前后检查数据
## 检查前后数据的区别
#### raw counts, same as pbmc@assays$RNA@counts[1:6, 1:6]
pbmc[["RNA"]]@counts[1:6, 1:6]
### library size normalized and log transformed data
pbmc[["RNA"]]@data[1:6, 1:6]
### scaled data
pbmc[["RNA"]]@scale.data[1:6, 1:6]

#scale是Seurat工作流程中必不可少的一步。但结果仅限于用作PCA分析的输入。
#ScaleData中默认设置是仅对先前标识的变量特征执行降维（默认为2000）.因此，在上一个函数调用中应省略features参数。
pbmc <- ScaleData(pbmc, vars.to.regress = "percent.mt")

#主成分分析（PCA）是一种线性降维技术
pbmc <- RunPCA(pbmc, features = VariableFeatures(object = pbmc), verbose = FALSE)
#print(pbmc[["pca"]], dims = 1:5, nfeatures = 5)
p1<- DimPlot(pbmc, reduction = "pca")
p1



#细胞分群
pbmc <- FindNeighbors(pbmc, dims = 1:20)
pbmc <- FindClusters(pbmc, resolution = 0.5)

# Look at cluster IDs of the first 5 cells
head(Idents(pbmc), 5)

#运行非线性降维(UMAP/tSNE)
pbmc <- RunUMAP(pbmc, dims = 1:20)
pbmc<- RunTSNE(pbmc, dims = 1:20)

## after we run UMAP and TSNE, there are more entries in the reduction slot
pbmc@reductions
DimPlot(pbmc, reduction = "umap", label = TRUE)
DimPlot(pbmc, reduction = "tsne", label = TRUE)

# find all markers of cluster 2
cluster2.markers <- FindMarkers(pbmc, ident.1 = 2, min.pct = 0.25)

# find markers for every cluster compared to all remaining cells, report only the positive ones
pbmc.markers <- FindAllMarkers(pbmc, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
pbmc.markers %>%
  group_by(cluster) %>%
  slice_max(n = 2, order_by = avg_log2FC)

#plot 2d t-sne graph
cluster_table <- merge(Idents(pbmc),pbmc@reductions[["tsne"]]@cell.embeddings,by='row.names')
cluster_table2 <- merge(Idents(pbmc),pbmc@reductions[["umap"]]@cell.embeddings,by='row.names')
target_cluster = cluster_table[which(cluster_table$x == 1),]
other_cluster = cluster_table[which(cluster_table$x != 1),]
fig <-  plot_ly(type = 'scatter', mode = 'markers')
fig <- fig %>% add_markers(data = target_cluster ,x =  ~tSNE_1, y = ~tSNE_2,marker=list(opacity = 1,color='blue'),showlegend = F)
fig <- fig %>% add_markers(data = other_cluster ,x =  ~tSNE_1, y = ~tSNE_2,marker=list(opacity = 0.2,color='grey'),showlegend = F)

fig

fig2 <- plot_ly(type = 'scatter', mode = 'markers') %>% add_markers(data = cluster_table ,x =  ~tSNE_1, y = ~tSNE_2,split= ~x,showlegend = F)
fig2
#输出表格
#write.table(cluster_table,file='../DataBaseUI-Shiny/DemoData/clusters_embeddings_table/tsne.txt',sep = '\t',col.names = FALSE)
write.table(cluster_table2,file='../DataBaseUI-Shiny/DemoData/clusters_embeddings_table/umap.txt',sep = '\t',col.names = TRUE,row.names = TRUE)


plot <- FeaturePlot(pbmc, features = "MS4A1",reduction = 'tsne')
plot

count_matrix <- data.frame(pbmc[["RNA"]]@counts)
count_matrix_convert <- as.data.frame(t(count_matrix))
setDT(count_matrix_convert, keep.rownames = TRUE)[]
count_matrix_convert_replace<- data.frame(lapply(count_matrix_convert, function(x) {gsub("[.]",'-',x)}))
merge_expression <- merge(cluster_table2,count_matrix_convert_replace,by.x = "Row.names",by.y = "rn")


#count_matrix_head <- head(count_matrix)
#count_matrix_head_convert <- as.data.frame(t(count_matrix_head))
#setDT(count_matrix_head_convert, keep.rownames = TRUE)[]

#count_matrix_head_convert_replace<- data.frame(lapply(count_matrix_head_convert, function(x) {gsub("[.]",'-',x)}))
#merge_expression <- merge(cluster_table,count_matrix_head_convert_replace,by.x = "Row.names",by.y = "rn")


target_cluster = filter(merge_expression,'MT-ND3' !=0)
other_cluster = filter(merge_expression,'MT-ND3' ==0)


#fig <-  plot_ly(type = 'scatter', mode = 'markers')
#fig <- fig %>% add_markers(data = target_cluster ,x =  ~tSNE_1, y = ~tSNE_2,marker=list(opacity = 1,color='blue'),showlegend = F)
#fig <- fig %>% add_markers(data = other_cluster ,x =  ~tSNE_1, y = ~tSNE_2,marker=list(opacity = 0.2,color='grey'),showlegend = F)
#fig

#cols_remain<-c("Row.names","x","tSNE_1","tSNE_2","CCR7")
#newdata2<-merge_expression[ ,colnames(merge_expression) %in% cols_remain]


tc <- merge_expression %>% select(Row.names,x,UMAP_1,UMAP_2,CD1E) %>% filter(CD1E != 0)

tc2 <- merge_expression%>% select(Row.names,x,UMAP_1,UMAP_2,'CD1E') %>% filter('CD1E' == 0)
