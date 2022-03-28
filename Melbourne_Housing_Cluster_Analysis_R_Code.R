
#Setting the working directory

setwd("C:\\Users\\jonah.muniz\\OneDrive - Accenture\\Masters Program\\Unsupervised Learning\\Assignment_2_Muniz")

#Loading in the Melbourne housing data

my.data <- read.csv(file="Melbourne_housing_FULL.csv",head=TRUE,sep=",")
str(my.data)
head(my.data)

#Loading the necessary R packages
require(cluster)
require(useful)
require(Hmisc)
library(HSAUR)
library(MVA)
library(HSAUR2)
library(fpc)
library(mclust)
library(lattice)
library(car)
require(ggplot2)
library(tidyverse)
library(Rtsne)


# Data cleaning to get a robust dataset for analysis

houses = my.data[complete.cases(my.data),]
print(str(houses))
workdata = houses[,c("Rooms","Price","Distance","Bedroom2",
                     "Bathroom","Car","Landsize","BuildingArea","YearBuilt")]
print(str(workdata))

#EDA
ggplot(workdata, aes(x=Rooms, y=Price)) + 
  geom_point() +
  ggtitle("Scatter Plot Rooms vs Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(workdata, aes(x=Bedroom2, y=Price)) + 
  geom_point() + 
  ggtitle("Scatter Plot Bedroom2 vs Price") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

#Data cleansing
workdata$Distance <- as.numeric(workdata$Distance) #Converting distance from char to num to enable scaling
workdata_cleaned <- workdata %>% distinct()
houses_cleaned <- houses %>% distinct()


# elbow plot
subdat <- workdata_cleaned
TSS <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
TSS
require(fpc)
complete3 <- cutree(hclust(hier.dist),3)
WSS <- cluster.stats(hier.dist,complete3, alt.clustering=NULL)$within.cluster.ss
WSS
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

wssplot <- function(workdata_cleaned, nc=15, seed=1234) {
  wss <- (nrow(workdata_cleaned)-1)*sum(apply(workdata_cleaned,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(workdata_cleaned, centers=i)$withinss)}
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")} 

wssplot(workdata_cleaned)

#Hierarchical Clustering 
hier.dist = dist(workdata_cleaned)
require(maptree)
hclustmodel <- hclust(hier.dist, method = 'complete')
plot(hclustmodel)

# choose the number of clusters k = 3
cut.3 <- cutree(hclustmodel, k=3)
head(cut.3)
cut.3
hier.ds <- cbind(workdata_cleaned,cut.3)
hier.ds

#Hier TSNE

set.seed(1) # for reproducibility
tsne <- Rtsne(workdata_cleaned , dims = 2, perplexity=80, verbose=TRUE, max_iter = 500, learning = 200)
tsne
colors = rainbow(length(unique(hier.ds$cut.3)))
names(colors) = unique(hier.ds$cut.3)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=2, cex.lab=1.5)
text(tsne$Y, labels = hier.ds$cut.3, col = colors[hier.ds$cut.3])

# train and plot using different parameters
tsne_plot <- function(perpl=30,iterations=500,learning=200){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(workdata_cleaned, dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning)
  text(tsne$Y, labels = hier.ds$cut.3, col = colors[hier.ds$cut.3])
  }

perplexity_values <- c(80,110,150)
sapply(perplexity_values, function(i){tsne_plot(perpl=i)})


# kmeans clustering with k=3 clusters

clusterresults <- kmeans(workdata_cleaned,3)
names(clusterresults)
BetSSPer <- clusterresults$betweenss/clusterresults$totss
BetSSPer
clusterresults$totss
clusterresults$centers
plot(clusterresults)

# cluster plots for kmeans

library(cluster) 
clusplot(workdata_cleaned, clusterresults$cluster, color=TRUE, shade=TRUE, lines=0)
k.clust <- cbind(workdata_cleaned, clusterresults$cluster)

# K-Means TSNE

set.seed(1) # for reproducibility
tsne <- Rtsne(workdata_cleaned , dims = 2, perplexity=80, verbose=TRUE, max_iter = 500, learning = 200)
tsne
colors = rainbow(length(unique(clusterresults$cluster)))
names(colors) = unique(clusterresults$cluster)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=2, cex.lab=1.5)
text(tsne$Y, labels = clusterresults$cluster, col = colors[clusterresults$cluster])

# train and plot using different parameters
tsne_plot <- function(perpl=80,iterations=500,learning=200){
  set.seed(1) # for reproducibility
  tsne <- Rtsne(workdata_cleaned, dims = 2, perplexity=perpl, verbose=TRUE, max_iter=iterations, eta=learning)
  plot(tsne$Y, t='n', main = print(paste0("perplexity = ",perpl, ", max_iter = ",iterations, ", learning rate = ",learning)), xlab="tSNE dimension 1", ylab="tSNE dimension 2", cex.main=1, cex.lab=1.5)
  text(tsne$Y, labels = clusterresults$cluster, col = colors[clusterresults$cluster])
}

perplexity_values <- c(50,80,100,120,150)
sapply(perplexity_values, function(i){tsne_plot(perpl=i)})





