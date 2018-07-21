##### Final-1 Clustering #####
load("C:/Users/nohah/Desktop/Sessions/NCCU_STAT/106-2/多變量分析/Final/Final_1/workspace_final1.RData")
#
seeds <- read.table("seeds.txt",header = T)

### Hierarchical clustering  ###---------------------------------
seeds2 <- seeds[,1:7]
library(cluster) 
x <- daisy(seeds2, stand=T) 
seed$Y %>% table
## Single-Linkage
agn1 <- agnes(x,metric="euclidean",method="single")  #有太多noise做得不好
plot(agn1,which.plots=2,main = "Single-Linkage")  
rect.hclust(agn1,k=3)
#abline(h=1.048,col=2)
agn1$ac  # AC=0.6074219 
seeds[,8][agn1$order]  # Single-Linkage


## Complete-Linkage
agn2 <- agnes(x,metric="euclidean",method="complete")  
plot(agn2,which.plots=2,main = "Complete-Linkage") 
rect.hclust(agn2,k=3)
#abline(h=7,col=2)
agn2$ac  # AC=0.9218714
seeds[,8][agn2$order]  # Complete-Linkage

cut.h.cluster <- cutree(agn2, k=3)

seeds[,8][which(cut.h.cluster == 1)] %>% table 
seeds[,8][which(cut.h.cluster == 2)] %>% table 
seeds[,8][which(cut.h.cluster == 3)] %>% table 
1-(20+17)/205  # cluster accu. rate

## Ward's Criterion
agn3 <- agnes(x,metric="euclidean",method="ward")  # AC=0.99 higer the better
plot(agn3, which.plots=2,main = "Ward's Criterion")
rect.hclust(agn3,k=3)
#abline(h=20,col=2)
agn3$ac  # AC= 0.9841322
seeds[,8][agn3$order]  # Ward's Criterion

cut.h.cluster <- cutree(agn3, k=3)

seeds[,8][which(cut.h.cluster == 1)] %>% table 
seeds[,8][which(cut.h.cluster == 2)] %>% table 
seeds[,8][which(cut.h.cluster == 3)] %>% table 
1-(9+8+3)/205

## Average-Linkage
agn4 <- agnes(x,metric="euclidean",method="average")  # AC=0.99 higer the better
plot(agn4, which.plots=2,main = "Average-Linkage") 
rect.hclust(agn4,k=3)
? rect.hclust
#abline(h=4,col=2)
agn4$ac  # AC=0.8668874
seeds[,8][agn4$order]  # Average-Linkage

cut.h.cluster <- cutree(agn4, k=3)

seeds[,8][which(cut.h.cluster == 1)] %>% table 
seeds[,8][which(cut.h.cluster == 2)] %>% table 
seeds[,8][which(cut.h.cluster == 3)] %>% table 



### Partitioning Method ###--------------------------------------
# K-medoid
pa <- pam(daisy(seeds,stand=T),3,diss=T)  # 分三群, 標準化, 嘗試不同群數看SC會不會更好
plot(pa,which.plots = 1)  
plot(pa,which.plots = 2)  


plot(pcs.seeds[,1:2], type="n") 
text(pcs.seeds,as.character(pa$clustering),col=pa$clustering,cex=0.6) 

pa$clustering  # K-medoids
seeds[,8][which(pa$clustering == 1)] %>% table 
seeds[,8][which(pa$clustering == 2)] %>% table 
seeds[,8][which(pa$clustering == 3)] %>% table 

1-(5+1)/205


### Self-Organizing Maps (SOM) ###---------------------------------
# install.packages('som') 
library(som) 
library(kohonen)
n.seeds <- normalize(seeds2, byrow=F)  # Standardize variables

seeds.som <- som(n.seeds,grid = somgrid(10,10, "hexagonal"))  # 先確datasize 
plot(seeds.som,type="mapping",labels=seeds[,8]) 
plot(seeds.som, type="dist.neighbours", main = "SOM neighbour distances") 

# display for 3 cluster by highest SC of pam
som.hc <- cutree(hclust(dist(seeds.som$codes[[1]])), 3) 
add.cluster.boundaries(seeds.som,som.hc) 
table(as.vector(cutree(hclust(dist(seeds.som$codes[[1]])), 3)  ),seeds[,8])

#  iteration = 205
seeds.som2 <- som(n.seeds, grid = somgrid(14, 14, "hexagonal"), rlen=205) 
plot(seeds.som2, type="dist.neighbours", main = "SOM neighbour distances") 
som.hc2 <- cutree(hclust(dist(seeds.som2$codes[[1]])), 3) 
add.cluster.boundaries(seeds.som2,som.hc2)
som.hc2 %>% as.vector()

seeds[,8][which(som.hc2 == 1)] %>% table 
seeds[,8][which(som.hc2 == 2)] %>% table 
seeds[,8][which(som.hc2 == 3)] %>% table 

