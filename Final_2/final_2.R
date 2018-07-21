##### Final_2 Multidimensional Scaling (MDS) #####
load("C:/Users/nohah/Desktop/Sessions/NCCU_STAT/106-2/多變量分析/code/MDS/workspace_MDS.RData")
#
seed <- read.table("seeds.txt",header = TRUE)
variris <- apply(seed[,-8],2,var)  # get var for all x's
seed.adjusted <- sweep(seed[,-8],2,sqrt(variris),"/")  # standardize

library(MVN)
mvn(seed[,-8])

# Torgerson-Gower scaling
seed.scal <- cmdscale(dist(seed.adjusted),k=2,eig=T)  # 內積,維度2minimizing the loss function “STRAIN”

# see how clustering works
library(MASS)
eqscplot(seed.scal$points,type="n",main="Torgerson-Gower MDS")
text(seed.scal$point,names(seed[,8]),cex=.8)  # looks like 2 group
# 這邊要看這裡分群跟原本類別是否有分出來

#
seed.scal$GOF  # proportion of variance explained by the first 2 dimensions (same as how we explain in PCA):
# this shows a good fit

# 看變數之間的相似度，用變數之間的corrleation 
variable.scal <- cmdscale(1/cor(seed[,-8]),k=2,eig=T)
eqscplot(variable.scal$points,type="n")
text(variable.scal$point,row.names(cor(seed[,-8])),cex=.8) # petal.width&petal.length是相似的

# nonmetric scaling isotonic regression
library(MASS)
seed.iso <- isoMDS(dist(seed.adjusted))  
eqscplot(seed.iso$points,type="n",main="Kruskal and Shepard (using isotonic regression)")
text(seed.iso$points,label=row.names(seed),cex=.8)
seed.iso$stress  # shows good fit 
#
scree.plot = function(d, k) {
  stresses=isoMDS(d, k=k)$stress
  for(i in rev(seq(k-1)))  
    stresses=append(stresses,isoMDS(d, k=i)$stress)
  plot(seq(k),rev(stresses), type="b", xaxp=c(1,k, k-1), ylab="Stress", xlab="Number of dimensions")
}
scree.plot(dist(seed.adjusted), k=6)  # 六個維度看下降幅度 因為只有四個變數所以四以後都一樣

# Shepard diagram for a 2D solution
seed.sh<-Shepard(dist(seed.adjusted), seed.iso$points, p=2)
plot(seed.sh, pch=".")
lines(seed.sh$x, seed.sh$yf, type = "S",col=2)
# 垂直: 二維度dis 水平: 原dist

# nonlinear mapping
seed.sammon <- sammon(dist(seed.adjusted),k=2)
eqscplot(seed.sammon$points,type="n",main="Non-metric MDS with Nonlinear Mapping")
text(seed.sammon$points,label=row.names(seed),cex=.8)
seed.sammon$stress  # 1.65% perfect fit

scree.plot = function(d, k) {
  stresses=sammon(d, k=k)$stress
  for(i in rev(seq(k-1)))  
    stresses=append(stresses,sammon(d, k=i)$stress)
  plot(seq(k),rev(stresses), type="b", xaxp=c(1,k, k-1), ylab="Stress", xlab="Number of dimensions")
}

# 
scree.plot(dist(seed.adjusted), k=4)
seed.sh <- Shepard(dist(seed.adjusted), seed.sammon$points, p=2)
plot(seed.sh, pch=".",col="black")
lines(seed.sh$x, seed.sh$yf, type = "S",col="red")


# code on website--------------------------------------------
### MDS ###
a <- seed[,-8]

# Load required packages
library(magrittr)
library(dplyr)
library(ggpubr)
# Cmpute MDS
mds <- a %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          main="Torgerson-Gower MDS",
          size = 1,
          repel = TRUE)
#
# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(a),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

### non-metric MDS ###
# Cmpute MDS
library(MASS)
mds <- a %>%
  dist() %>%          
  isoMDS() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          main="Kruskal’s Non-metric MDS",
          size = 1,
          repel = TRUE)
# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(a),
          main="Non-metric MDS",
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)




### Sammon’s Non-linear Mapping ###
# Cmpute MDS
mds <- a %>%
  dist() %>%          
  sammon() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2",
          main="Sammon’s Non-linear Mapping",
          size = 1,
          repel = TRUE)

# K-means clustering
clust <- kmeans(mds, 3)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust)
# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(a),
          main="Sammon’s Non-linear Mapping",
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

