##IMPORT LIBRARIES AND DATA

#import libraries
library(FactoMineR)
library(factoextra)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(car)
library(carData)
library(psych)
library(tidyverse)
library(cluster)
library(caret)
library(MASS)
library(GPArotation)

#import data WHR 2019
whr <- read_csv("C:/Users/Windy/OneDrive/Desktop/WHR/2019.csv")
head(whr)
data1<-whr[,4:9]
View(whr)

##PCA
pc1 = prcomp(data1, scale. = T, center = T)
summary(pc1)

# Variance explained
fviz_eig(pc1,addlabels = TRUE)

# 3D representation for individuals
library(scatterplot3d)
scatterplot3d(pc1$x[,1],pc1$x[,2],pc1$x[,3],surface = FALSE)

# Extract the results for variables
var <- get_pca_var(pc1)

# Variables contributions to PC1
fviz_contrib(pc1, choice = "var", axes = 1, top = 10)

# Variables contributions to PC2
fviz_contrib(pc1, choice = "var", axes = 2, top = 10)


##CLUSTERING

whr.scale = scale(whr[,4:9])

# Compute total within-cluster sum of squares
#The Elbow Method
fviz_nbclust(whr.scale, kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")

#The Silhouette plot
fviz_nbclust(whr.scale, kmeans, method = "silhouette", k.max = 24) + theme_minimal() + ggtitle("The Silhouette Plot")


# Execution of k-means with k=3
final1 <- kmeans(whr.scale, 3)
fviz_cluster(final1, data = whr.scale) + theme_minimal() + ggtitle("k = 3")
# K-means clusters showing the group of each individuals
final1$size
final1$cluster

# Cluster Characteristics
kmean = kmeans(whr.scale,3)
kmean$centers

# Cluster Membership
tabelcluster<-as.data.frame(whr$`Country or region`)
tabelcluster$cluster <- factor(final1$cluster)
View(tabelcluster)

