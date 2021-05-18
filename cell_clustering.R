## Libraries----------------------------------------------------------------------------------------------

library(factoextra)
library(ggplot2)
library(cluster)
library(mclust)

## Load data----------------------------------------------------------------------------------------------

cell <- read.csv("D:/Y79_data.csv", header = TRUE)
attach(cell)
ggpairs(cell[-1])
ggplot(cell, aes(diameter, Vmb)) + geom_point()

## K-means clustering---------------------------------------------------------------------------------

# Function to compute total within-cluster sum of square
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

# plotting values for each cluster starting from 1 to 9
wssplot(cell[-1], nc = 9)

set.seed(1)
cluster.cell<- kmeans(cell[-1], 3, nstart = 20)
cluster.cell


# plot again according to clusters
cluster.cell$cluster <- as.factor(cluster.cell$cluster)
ggplot(cell, aes(diameter, Vmb, color=cluster.cell$cluster)) + geom_point() 


fviz_cluster(cluster.cell, data =cell[-1])



## Model based clustering-------------------------------------------------------------------------------


# Automatic best model and cluster count selection via BIC:
(cell.bestMBC <- Mclust(cell[-1]))

plot(cell.bestMBC, what=c("BIC", "classification"))
# the model selected is the VVI with 3 clusters

# Plot results
plot(cell.bestMBC, what = "density")
plot(cell.bestMBC, what = "uncertainty")

# forcing 2 clusters
(cell.2MBC <- Mclust(cell[-1],G=2))
plot(cell.2MBC, what=c("classification", "uncertainty", "density"))
