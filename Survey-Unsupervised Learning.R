# Project: Survey Unsupervised Clustering and PCA
# Full Name: Tan Wai Kit

# R code to import and prepare the dataset
dfdata1=read.table("EWCS_2016.csv",sep=",",header=TRUE)
dfdata1[,][dfdata1[, ,] == -999] <- NA
kk=complete.cases(dfdata1)
dfdata1=dfdata1[kk,]

summary(dfdata1)

# Scaling and Preparation of Data
dfdata2 <- na.omit(dfdata1)
dfdata3 <- scale(dfdata2)

# Plotting Correlation
library(GGally)
ggcorr(dfdata3, name = "Correlation", label = T, label_round = 2,
       low = "#FFFFFF",
       mid = "#FFFFFF",
       high = "#0083EF")

# Principal Component Analysis
dfpca <- prcomp(dfdata3)
summary(dfpca)
library(ggfortify)
autoplot(prcomp(dfdata3, scale = T),  loadings.label = T, label = F)

loadings <- dfpca$rotation
print(loadings)
component.sd <- dfpca$sdev
print(component.sd)
fun <- function(loading, component.sd){component.sd*loading}
explained <- t(apply(loadings, 1, fun, component.sd))
explained[, 1:4]

# Adjusting PCA in positive direction
loadings=-loadings
print(loadings)

dfVE <- component.sd^2
print(dfVE)
dfPVE <- dfVE/sum(dfVE)
print(dfPVE)
par(mfrow = c(1,2))
plot(dfPVE, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0,1),type='b')
plot(cumsum(dfPVE), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1),type='b')

# K-Means Clustering
library(tidyverse)
library(factoextra)
library(cluster)

# Selecting number of clusters
set.seed(1908)
fviz_nbclust(dfdata3, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 3) + labs(title = "Optimal number of cluster using Elbow Method")
dfPVE[1] + dfPVE[2] + dfPVE[3]
dfPVE[1] + dfPVE[2] + dfPVE[3] + dfPVE[4]
dfPVE[1] + dfPVE[2] + dfPVE[3] + dfPVE[4] + dfPVE[5]

# K = 4
set.seed(1908)
dfk4 <- kmeans(dfdata3, centers = 4, nstart = 25)
dfk4
summary(dfk4)
str(dfk4)
fviz_cluster(dfk4, data = dfdata3)
fviz_cluster(dfk4, geom = "point", data = dfdata3) + ggtitle("k = 4")
plot1 <- fviz_cluster(dfk4, geom = "point", data = dfdata3) + ggtitle("k = 4")
print(aggregate(dfdata2, by=list(dfk4$cluster), FUN=mean))

# K = 3
set.seed(1908)
dfk3 <- kmeans(dfdata3, centers = 3, nstart = 25)
dfk3
summary(dfk3)
fviz_cluster(dfk3, data = dfdata3)
fviz_cluster(dfk3, geom = "point", data = dfdata3) + ggtitle("k = 3")
plot2 <- fviz_cluster(dfk3, geom = "point", data = dfdata3) + ggtitle("k = 3")
print(aggregate(dfdata2, by=list(cluster=dfk3$cluster), mean))

# Comparing plots for K-means
library(gridExtra)
grid.arrange(plot1, plot2, nrow = 2)

# Hierarchical Clustering
fviz_nbclust(dfdata3, FUN = hcut, method = "wss")

d <- dist(dfdata3, method = "euclidean")

dfhc.com <- hclust(d, method = "complete")
par(mfrow = c(1,1))
plot(dfhc.com , main = "Complete Linkage", xlab = "", sub = "",cex = .1)
cut.com <- cutree(dfhc.com, k = 4)
table(cut.com)
fviz_cluster(geom = "point", list(data = dfdata3, cluster = cut.com)) + ggtitle("Complete Linkage")
print(aggregate(dfdata2, by=list(cut.com), FUN=mean))

dfhc.avg <- hclust(d, method = "average")
par(mfrow = c(1,1))
plot(dfhc.avg , main = "Average Linkage", xlab = "", sub = "", cex = .1)
cut.avg <- cutree(dfhc.avg, k = 4)
table(cut.avg)
fviz_cluster(geom = "point", list(data = dfdata3, cluster = cut.avg)) + ggtitle("Average Linkage")
print(aggregate(dfdata2, by=list(cut.avg), FUN=mean))

dfhc.sin <- hclust(d, method = "single")
par(mfrow = c(1,1))
plot(dfhc.sin , main = "Single Linkage", xlab = "", sub = "", cex = .1)
cut.sin <- cutree(dfhc.sin, k = 4)
table(cut.sin)
fviz_cluster(geom = "point", list(data = dfdata3, cluster = cut.sin)) + ggtitle("Single Linkage")
print(aggregate(dfdata2, by=list(cut.sin), FUN=mean))

# Comparing plots
library(gridExtra)
plot3 <- fviz_cluster(geom = "point", list(data = dfdata3, cluster = cut.com)) + ggtitle("Complete Linkage")
plot4 <- fviz_cluster(geom = "point", list(data = dfdata3, cluster = cut.avg)) + ggtitle("Average Linkage")
plot5 <- fviz_cluster(geom = "point", list(data = dfdata3, cluster = cut.sin)) + ggtitle("Single Linkage")
grid.arrange(plot3, plot4, plot5, nrow = 2)

# Full comparison
grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow = 3)
