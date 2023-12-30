# Project 3
# Rick Lattin, Lydia Malone, Armin Charkhkar

# load data
myData <- read.csv("/Users/ricklattin/Documents/SMU Year 4 Sem 1/Data_Mining/Project_3_CLusters/google_review_ratings.csv")
colnames(myData)
dataTypes <- sapply(myData, class)
dataTypes

# drop column x and user name from the data
len <- length(myData)
myData <- myData[2:(len-1)]
colnames(myData)

# remove nas
myData <- na.omit(myData)

# change one character column to an integer
myData$"Category.11" <- as.numeric(myData$"Category.11")
head(myData)
dataTypes <- sapply(myData, class)
dataTypes

# remove all zeros
rows_w_zeros <- list()

# loop to find indexes of zeros
for(i in 1:nrow(myData)) { 
  zero_present <- FALSE
  
  for(j in 1:ncol(myData)){
    if(myData[i, j] == 0.0){
      zero_present <- TRUE
      break
    }
  }
  
  if(zero_present){
    rows_w_zeros[[length(rows_w_zeros)+1]] <- i
  }
}

# drops indexes of zeros
dim(myData)
for(x in rows_w_zeros){
  myData <- myData[-x, , drop = TRUE]
}
dim(myData)
myDataCleaned <- myData


# K-MEANS CLUSTERING------------------------------
km <- kmeans(myDataCleaned, 4, iter.max = 1000)
km$cluster # membership
km$centers #cluster centroids

?kmeans
totalDist <- NULL
for(k in 1:100){
  km <- kmeans(myDataCleaned, k, iter.max = 1000)
  totalDist[k] <- km$tot.withinss
}
plot(totalDist)

km <- kmeans(myDataCleaned, 10, iter.max = 1000)

# plot using package: factoextra
install.packages("factoextra")
library(factoextra)

fviz_cluster(km, myDataCleaned)


#HIERARCHICAL CLUSTERING--------------------------
# install packages for sihouette
install.packages("dbscan")
library(dbscan)
install.packages("cluster")
library(cluster)

d <- dist(myDataCleaned, method = "euclidean")

hc <- hclust(d, method = "complete") # merge using centroid

# plot hierarchical
plot(hc)
rect.hclust(hc, k = 5)

totalDist_H <- NULL
for(x in 2:100){  
  hc_membership <- cutree(hc, k = x)
  hc_sil <- silhouette(hc_membership, dist(myDataCleaned))
  hc_sil_score <- summary(hc_sil)$avg.width
  totalDist_H[x] <- hc_sil_score
}
plot(totalDist_H)

hc_membership <- cutree(hc, k = 20)
hc_membership[1:10] # group membership for first 10 states
table(hc_membership)

#GUASSIAN MIXTURE MODEL-----------------------------
install.packages("mclust")
library(mclust)

# build clusters
?Mclust

totalDist_G <- NULL
for(x in 2:50){  
  gmm <- Mclust(myDataCleaned, x)  
  gmm_sil <- silhouette(gmm$classification, dist(myDataCleaned))
  gmm_sil_score <- summary(gmm_sil)$avg.width
  totalDist_G[x] <- gmm_sil_score
}
plot(totalDist_G)

gmm <- Mclust(myDataCleaned, 10) #number of clusters
gmm$classification # membership
gmm$parameters # mean, covariance per Gaussian

# use function fviz_cluster to plot
?fviz_cluster
fviz_cluster(gmm)

# DBSCAN ---------------------------------------------
install.packages("dbscan")
library(dbscan)

# create a dbscan
?dbscan
db <- dbscan(myDataCleaned, 3.0, 5)
db
str(db)

# printout cluster output
cbind(row.names(myDataCleaned), db$cluster)

# silhouette coefficients (for each data point)
install.packages("cluster")
library(cluster)
?silhouette

# compute pairwise distance between all data points
d <- dist(myDataCleaned)
silhouette(db$cluster, d)
plot(silhouette(db$cluster, d))



# GLOBAL METRICS
install.packages("ggplot2")
install.packages("factoextra")
install.packages("fpc")
library(ggplot2) 
library(factoextra)
library(fpc)

# Silhouette Plots

# K-Means Clustering
km_sil <- silhouette(km$cluster, dist(myDataCleaned))
fviz_silhouette(km_sil) + ggtitle("Silhouette Plot for K-Means Clustering")
km_sil_score <- summary(km_sil)$avg.width

# Hierarchical Clustering 
hc_sil <- silhouette(hc_membership, dist(myDataCleaned))
fviz_silhouette(hc_sil) + ggtitle("Silhouette Plot for Hierarchical Clustering")
hc_sil_score <- summary(hc_sil)$avg.width

# DBSCAN
db_sil <- silhouette(db$cluster, d)  # Calculate silhouette for DBSCAN
fviz_silhouette(db_sil) + ggtitle("Silhouette Plot for DBSCAN")
db_sil_score <- summary(db_sil)$avg.width

# Gaussian Mixture Model (GMM)
gmm_sil <- silhouette(gmm$classification, dist(myDataCleaned))
fviz_silhouette(gmm_sil) + ggtitle("Silhouette Plot for Gaussian Mixture Model (GMM)")
gmm_sil_score <- summary(gmm_sil)$avg.width

# Plot for all silhouette scores
silhouette_scores <- c("K-Means" = km_sil_score,
                       "Hierarchical" = hc_sil_score,
                       "DBSCAN" = db_sil_score,
                       "Gaussian Mixture" = gmm_sil_score)

# Create a data frame for silhouette scores
silhouette_data <- data.frame(Method = names(silhouette_scores),
                              Silhouette_Score = silhouette_scores)

# Create a bar plot
ggplot(silhouette_data, aes(x = Method, y = Silhouette_Score, fill = Method)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(y = "Silhouette Score", x = NULL, title = "Silhouette Scores for Clustering Methods") +
  geom_text(aes(label = round(Silhouette_Score, 2)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("K-Means" = "blue", "Hierarchical" = "red", "DBSCAN" = "green", "Gaussian Mixture" = "purple")) +
  theme_minimal() +
  theme(legend.position = "none")



# Calinhara index (the higher the better)

db_clusters <- db$cluster
ch_km <- calinhara(myDataCleaned, km$cluster)
ch_hc <- calinhara(myDataCleaned, hc_membership)
ch_db <- calinhara(myDataCleaned[db_clusters > 0, ], db_clusters[db_clusters > 0])
ch_gmm <- calinhara(myDataCleaned, gmm$classification)

# Create a data frame for the CH indices
ch_data <- data.frame(Method = c("K-Means", "Hierarchical", "DBSCAN", "Gaussian Mixture"),
                      CH_Index = c(ch_km, ch_hc, ch_db, ch_gmm))

# Create barplot
ggplot(ch_data, aes(x = Method, y = CH_Index, fill = Method)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(y = "Calinski-Harabasz Index", x = NULL, title = "Calinski-Harabasz Index for Clustering Algorithms") +
  geom_text(aes(label = round(CH_Index, 2)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("K-Means" = "blue", "Hierarchical" = "red", "DBSCAN" = "green", "Gaussian Mixture" = "purple")) +
  theme_minimal() +
  theme(legend.position = "none")



# Davies-Bouldin Index (the lower the better)

# K-Means clustering
db_index_km <- cluster.stats(dist(myDataCleaned), km$cluster)$dunn
db_data_km <- data.frame(Method = "K-Means", DB_Index = db_index_km)

# Hierarchical clustering
db_index_hc <- cluster.stats(dist(myDataCleaned), hc_membership)$dunn
db_data_hc <- data.frame(Method = "Hierarchical", DB_Index = db_index_hc)

# DBSCAN
db_index_db <- cluster.stats(dist(myDataCleaned[db_clusters > 0, ]), db_clusters[db_clusters > 0])$dunn
db_data_db <- data.frame(Method = "DBSCAN", DB_Index = db_index_db)

# Gaussian Mixture Model
db_index_gmm <- cluster.stats(dist(myDataCleaned), gmm$classification)$dunn
db_data_gmm <- data.frame(Method = "Gaussian Mixture", DB_Index = db_index_gmm)

# Create barplot 
db_data_combined <- rbind(db_data_km, db_data_hc, db_data_db, db_data_gmm)
ggplot(db_data_combined, aes(x = Method, y = DB_Index, fill = Method)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(y = "Davies-Bouldin Index (DB Index)", x = NULL, title = "DB Index for Clustering Methods") +
  geom_text(aes(label = round(DB_Index, 2)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("K-Means" = "blue", "Hierarchical" = "red", "DBSCAN" = "green", "Gaussian Mixture" = "purple")) +
  theme_minimal() +
  theme(legend.position = "none")







