##### Final_4 Factor Analysis #####
load("C:/Users/nohah/Desktop/Sessions/NCCU_STAT/106-2/多變量分析/Final/Final_4/workspace_final4.RData")

#
library(readxl)

#
data <- read_excel("European_Jobs.xlsx") 
data1 <- data[,-1]  

corMat <- cor(data1)
corMat

### 1 factor
spearman.mle <- factanal(covmat=as.matrix(corMat),factors=1,n.obs = 26)  # set factor is 1, obs given for max. factor 
spearman.mle  # 1 factor is sufficient / should check the assumption, but u can't check by corr. matrix
# but it's hard to explain for 1 factor, that represent for everything
1-spearman.mle$uniq  # to see communalities, the higher the better, Music & Discrimination is not good enough
# so we then try 2 factors

### 2 factor
spearman.mle2 <- factanal(covmat=as.matrix(corMat),factors=2,n.obs=26)
spearman.mle2  # factor_1 = - Disc./ factor_2 = Disc. 
1-spearman.mle2$uniq  # uniquness of Math & Music is not good


subnames <- names(data1)
l <- loadings(spearman.mle2)
plot(l[,1],l[,2],type="n",xlab="Factor 1",ylab="Factor 2",xlim=c(0,1),ylim=c(0,1))
text(l[,1],l[,2],subnames)
abline(v=0.4,lty=2);abline(h=0.4,lty=2)  # the perfect: two group be right down & left top /It has rotation

### do a not "promax" rotation , not orthogonal rotation
spearman.mle3 <- factanal(covmat=as.matrix(corMat),factors=2,rotation="promax",n.obs=26)
spearman.mle3  
1-spearman.mle3$uniq
l <- loadings(spearman.mle3)
plot(l[,1],l[,2],type="n",xlab="Factor 1",ylab="Factor 2")
text(l[,1],l[,2],subnames)
abline(v=0.4,lty=2);abline(h=0.4,lty=2)


#
x <- read.table("citycrime.txt",h=T)
x %>% str
data1 %<>% as.data.frame()
rownames(data) <- data[,1] %>% as.vector()
data[,1] %>% as.vector()
library(magrittr)
# install.packages('robCompositions')
library(robCompositions)
factor <- pfa(data1,factors=2,scores="Bartlett")  # find fators by MLE method first
biplot(factor)
