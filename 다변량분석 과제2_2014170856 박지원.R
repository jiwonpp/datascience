##MDA Assignment#2 Clustering(KMC, HC, DBSCAN)
#Packages
library(ISLR)
library(clValid)
library(plotrix)
library(tictoc)
library(ggfortify)
library(gplots) 
library(dendextend)
library(factoextra)
library(dbscan)
library(FactoMineR)
library(RColorBrewer) 


#Load the College dataset
data(College)
View(College)

#Remove the private column
College_x <- College[,-1]

#KMC-----------------------------------------------------------------------------------
#Standardization
College_x_scaled <- scale(College_x, center = TRUE, scale = TRUE)

#Q1-1
# Evaluating the cluster validity measures k=2~10
tic()
College_cvalid <- clValid(College_x_scaled, 2:10, clMethods = "kmeans", 
                        validation = c("internal", "stability"))
toc()
summary(College_cvalid)

#Q1-2 
# Perform K-Means Clustering with k=3
cluster_size <- data.frame()  #dataframe for cluster size
for (i in 1:10){
  College_kmc <- kmeans(College_x_scaled,3)
  #str(College_kmc) 
  print (College_kmc$centers)
  cluster_size <- rbind(cluster_size, College_kmc$size)
}
cluster_size

#Q1-3
# Perform K-Means Clustering with k=10
cluster_size <- data.frame()
for (i in 1:10){
  College_kmc <- kmeans(College_x_scaled,10)
  #str(College_kmc) 
  print (College_kmc$centers)
  cluster_size <- rbind(cluster_size, College_kmc$size)
}
cluster_size

#Q1-4  Radar chart
# Compare each cluster for KMC
cluster_kmc <- data.frame(College_x_scaled, clusterID = as.factor(College_kmc$cluster))
kmc_summary <- data.frame()

for (i in 1:(ncol(cluster_kmc)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(cluster_kmc[,i], cluster_kmc$clusterID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3))
rownames(kmc_summary) <- colnames(College_x)
kmc_summary  ##same as centroid!!

par(mfrow = c(1,3))
for (i in 1:3){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(kmc_summary[,i], labels = rownames(kmc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 3, show.grid.labels=1)}
dev.off() 


#Q1-4 T-test
kmc_cluster1 <- College_x[College_kmc$cluster == 1,]
kmc_cluster2 <- College_x[College_kmc$cluster == 2,]
kmc_cluster3 <- College_x[College_kmc$cluster == 3,]

# Compare the first and the second cluster #comparing the same data - no normalization
# t_test_result
kmc_t_result <- data.frame()

for (i in 1:13){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "two.sided")$p.value
  #cluster 1 >cluster2
  kmc_t_result[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster2[,i], 
                              alternative = "less")$p.value
}

kmc_t_result

# Compare the first and the third cluster #comparing the same data - no normalization
# t_test_result
kmc_t_result <- data.frame()

for (i in 1:13){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster1[,i], kmc_cluster3[,i], 
                              alternative = "two.sided")$p.value
  #cluster 1 >cluster3
  kmc_t_result[i,2] <- t.test(kmc_cluster1[,i], kmc_cluster3[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result[i,3] <- t.test(kmc_cluster1[,i], kmc_cluster3[,i], 
                              alternative = "less")$p.value
}

kmc_t_result

# Compare the third and the second cluster #comparing the same data - no normalization
# t_test_result
kmc_t_result <- data.frame()

for (i in 1:13){
  
  kmc_t_result[i,1] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                              alternative = "two.sided")$p.value
  #cluster 2 >cluster3
  kmc_t_result[i,2] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                              alternative = "greater")$p.value
  
  kmc_t_result[i,3] <- t.test(kmc_cluster2[,i], kmc_cluster3[,i], 
                              alternative = "less")$p.value
}

kmc_t_result


#Q1-5 Visualization
set.seed(1)
autoplot(kmeans(College_x_scaled, 3), data = College_x_scaled)




# Part 2: Hierarchical Clustering -----------------------------------------
#Q2-1 Evaluating the cluster validity measures k=2~10
tic()
College_cvalid <- clValid(College_x_scaled, 2:10, clMethods = "hierarchical", 
                          validation = c("internal", "stability"))
toc()
summary(College_cvalid)

#Q2-2 draw dendrogram
# Compute the similarity using the spearman coefficient
cor_Mat <- cor(t(College_x_scaled), method = "spearman")
dist_College <- as.dist(1-cor_Mat) ##higher correlation means shorter distance

# Perform hierarchical clustering using complete method
hr1 <- hclust(dist_College, method = "complete", members=NULL)
plot(as.dendrogram(hr1), edgePar=list(col=5, lwd=2), horiz=T)

# Perform hierarchical clustering using ward.D method
hr2 <- hclust(dist_College, method = "ward.D", members=NULL)
plot(as.dendrogram(hr2), edgePar=list(col=5, lwd=2), horiz=T)

# Perform hierarchical clustering using ward.D2 method
hr3 <- hclust(dist_College, method = "ward.D2", members=NULL)
plot(as.dendrogram(hr3), edgePar=list(col=5, lwd=2), horiz=T)

# Perform hierarchical clustering using single method
hr4 <- hclust(dist_College, method = "single", members=NULL)
plot(as.dendrogram(hr4), edgePar=list(col=5, lwd=2), horiz=T)

# Perform hierarchical clustering using average method
hr5 <- hclust(dist_College, method = "average", members=NULL)
plot(as.dendrogram(hr5), edgePar=list(col=5, lwd=2), horiz=T)

# Perform hierarchical clustering using mcquitty method
hr6 <- hclust(dist_College, method = "mcquitty", members=NULL)
plot(as.dendrogram(hr6), edgePar=list(col=5, lwd=2), horiz=T)

# Perform hierarchical clustering using centroid method
hr7 <- hclust(dist_College, method = "centroid", members=NULL)
plot(as.dendrogram(hr7), edgePar=list(col=5, lwd=2), horiz=T)


#Q2-3
# Find the clusters
mycl <- cutree(hr1, k=10)
plot(hr1)
rect.hclust(hr1, k=10, border="red")

# Compare each cluster for HC
College_hc <- data.frame(College_x_scaled, 
                       clusterID = as.factor(mycl))
hc_summary <- data.frame()

for (i in 1:(ncol(College_hc)-1)){
  hc_summary = rbind(hc_summary, 
                     tapply(College_hc[,i], College_hc$clusterID, mean))
}

colnames(hc_summary) <- paste("cluster", c(1:10))
rownames(hc_summary) <- c(colnames(College_x), "LoanRatio")
hc_summary

# Radar chart
par(mfrow = c(2,5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(hc_summary[,i], labels = rownames(hc_summary), 
              radial.lim=c(-2,2), rp.type = "p", main = plot_title, 
              line.col = "pink", lwd = 2, show.grid.labels=1)
}
dev.off()


# Compare the cluster 3 & 8
hc_cluster3 <- College_hc[College_hc$clusterID == 3, c(1:11)]
hc_cluster8 <- College_hc[College_hc$clusterID == 8, c(1:11)]

# t_test_result
hc_t_result <- data.frame()

for (i in 1:11){
  
  hc_t_result[i,1] <- t.test(hc_cluster3[,i], hc_cluster8[,i], 
                             alternative = "two.sided")$p.value
  
  hc_t_result[i,2] <- t.test(hc_cluster3[,i], hc_cluster8[,i], 
                             alternative = "greater")$p.value
  
  hc_t_result[i,3] <- t.test(hc_cluster3[,i], hc_cluster8[,i], 
                             alternative = "less")$p.value
}

hc_t_result



# Compare the cluster 1 & 8
hc_cluster1 <- College_hc[College_hc$clusterID == 1, c(1:11)]
hc_cluster8 <- College_hc[College_hc$clusterID == 8, c(1:11)]

# t_test_result
hc_t_result <- data.frame()

for (i in 1:11){
  
  hc_t_result[i,1] <- t.test(hc_cluster1[,i], hc_cluster8[,i], 
                             alternative = "two.sided")$p.value
  
  hc_t_result[i,2] <- t.test(hc_cluster1[,i], hc_cluster8[,i], 
                             alternative = "greater")$p.value
  
  hc_t_result[i,3] <- t.test(hc_cluster1[,i], hc_cluster8[,i], 
                             alternative = "less")$p.value
}

hc_t_result


#Q2-4 heatmap
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
some_col_func <- function(n) rev(colorspace::heat_hcl(n, c = c(80, 30), l = c(30, 90), power = c(1/5, 1.5)))
d_c <- dist(College_x_scaled) 
hc_c <- hclust(d_c, method = "complete")
dend <- as.dendrogram(hc_c)
gplots::heatmap.2(as.matrix(College_x_scaled), 
                  main = "Heatmap",
                  srtCol = 20,
                  dendrogram = "row",
                  Rowv = dend,
                  Colv = "NA",
                  trace="none",          
                  margins =c(5,0.1),      
                  key.xlab = "Cm",
                  denscol = "grey",
                  density.info = "density",
                  col = some_col_func
)


# Part 3: Density-based Clustering -----------------------------------------
ploan <- read.csv("Personal Loan.csv")
ploan_x <- ploan[,-c(1,5,10)]
View(ploan_x)
#Standardization
ploan_x_scaled <- scale(ploan_x, center = TRUE, scale = TRUE)

set.seed(12345) ##fix randomness

#Q3-1
# DBSCAN
for (i in (1:5)){
  for (j in (1:5)) {
    DBSCAN_multishapes <- dbscan(ploan_x_scaled, eps = i, minPts = j)##notice the parameter
    print(DBSCAN_multishapes)
  }
}

#Q3-2 Radar chart
DBSCAN_multishapes <- dbscan(ploan_x_scaled, eps = 3, minPts = 1)
cluster_dbscan <- data.frame(ploan_x_scaled, clusterID = as.factor(DBSCAN_multishapes$cluster))
dbscan_summary <- data.frame()

for (i in 1:(ncol(cluster_dbscan)-1)){
  dbscan_summary = rbind(dbscan_summary, 
                      tapply(cluster_dbscan[,i], cluster_dbscan$clusterID, mean))
}

colnames(dbscan_summary) <- paste("cluster", c(1:10))
rownames(dbscan_summary) <- colnames(ploan_x)
dbscan_summary  

par(mfrow = c(2,5))
for (i in 1:10){
  plot_title <- paste("Radar Chart for Cluster", i, sep=" ")
  radial.plot(dbscan_summary[,i], labels = rownames(dbscan_summary), 
              radial.lim=c(-2,3), rp.type = "p", main = plot_title, 
              line.col = "red", lwd = 2, show.grid.labels=1)}
dev.off() 

#Q3-3 PCA and cluste rplot
res.pca <- PCA(ploan_x_scaled, graph = FALSE) #find two axis
res.pca$eig
#Draw cluster plot
DBSCAN_multishapes <- dbscan(ploan_x_scaled, eps = 3, minPts = 2)
fviz_cluster(DBSCAN_multishapes, ploan_x_scaled, ellipse = FALSE, geom = "point",
             show.clust.cent = FALSE)

