##### Mid report #####
load("C:/Users/nohah/Desktop/Sessions/NCCU_STAT/106-2/多變量分析/Midterm/workspace_mid.RData")
# Data
library(readxl)
library(magrittr)
European_Jobs <- read_excel("European_Jobs.xlsx")  
glass <- read.table("glass.txt",header = F)
new_wages <- read.table("new_wages.txt",header = T,sep = " ")

glass$V10 %<>% as.factor() 
glass %>% str
colnames(glass) <- c("refractive_index","Sodium","Magnesium","Aluminum",
                     "Silicon","Potassium","Calcium","Barium","Iron","Type")

##### Q1 European_Jobs PCA #####
### PCA ###
# data
European_Jobs <- read_excel("European_Jobs.xlsx")
str(European_Jobs)
europ <- European_Jobs[,-1]
pairs(europ)
findLinearCombos(European_Jobs[,-1])
#
library(stats)
pca1 <- princomp(scale(europ,scale=TRUE,center=TRUE),cor=TRUE)
#center data with mean =0. scale=T means standard deviation is set 1.
plot(pca1) #Variation	explained
plot(pca1, type="line")
abline(h=1, col="blue") # Kaiser eigenvalue-greater-than-one rule, choose pc1~pc3 by Kaiser
summary(pca1)

## Standard deviation of components is represents the percent of variation each component explains
pca1$sdev
## Compute variance explained
pve=(pca1$sdev)^2 / (sum(pca1$sdev^2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b')#scree plot
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')

## pc1 vs pc2 plot
biplot(pca1,scale=T) #first two components
autoplot(prcomp(europ, center = TRUE, scale = TRUE), 
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 5)
print(pca1$loadings,digits = 6,cutoff = 0)
European_Jobs[c(18,21),]

### Permutation Test ###
sign.pc<-function(x,R=1000,m=length(x), cor=T,...){
  pc.out<-princomp(x,cor=cor,...)
  pve=(pc.out$sdev^2/m)[1:m]
  pve.perm<-matrix(NA,ncol=m,nrow=R)
  for(i in 1:R){
    x.perm<-apply(x,2,sample)
    pc.perm.out<-princomp(x.perm,cor=cor,...)
    pve.perm[i,]=(pc.perm.out$sdev^2/m)[1:m]
  }
  pval<-apply(t(pve.perm)>pve,1,sum)/R
  return(list(pve=pve,pval=pval))
}

library(RCurl)
library(bitops)
# 取前兩個comp.
sign.pc(europ,cor=T)  








##### Q2 European_Jobs CCA #####









##### Q3 glass #####
### training dataset & testing dataset ###
colnames(glass) <- c("refractive_index","Sodium","Magnesium","Aluminum",
                     "Silicon","Potassium","Calcium","Barium","Iron","Type")
glass$Type %>% table
attach(glass)
library(caret)
set.seed(500)
inTrain <- createDataPartition(y=glass$Type,p=0.7, list=FALSE)
training <- glass[inTrain,]
testing <- glass[-inTrain,]
pairs(glass)  # 沒有高度共線性
findLinearCombos(glass[,-10])  # 變數之間沒有共線性問題
training$Type %>% table
testing[,10]

### Classification Tree ### 
# caret
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
glass.carettree <- train(Type ~ .,method="rpart",data=training, 
                    trControl = control)

print(glass.carettree)  
plot(glass.carettree$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(glass.carettree$finalModel, use.n=TRUE, all=TRUE, cex=.8)

# predict 
(carettree.pred <- predict(glass.carettree, testing))




# rpart
training$Type %>% table
library(rpart)
library(rpart.plot) 
? rpart.control
glass.control <- rpart.control(minbucket = 4,minsplit = 15) # since there is 6 data in Type 6
glass.treeorig <- rpart(Type ~. ,data = training,method = "class",
                        control = glass.control)
prp(glass.treeorig,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # 2:number of correct classifications / number of observations in that node
    extra=2)  

printcp(glass.treeorig)  # apparent error : 29.17%
plotcp(glass.treeorig)

# prune the tree : min xerror is nsplit = 9
pfit<- prune(glass.treeorig, cp=   glass.treeorig$cptable[which.min(glass.treeorig$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
prp(pfit,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # 2:number of correct classifications / number of observations in that node
    extra=2)  

# LOOCV 
glass.control <- rpart.control(minbucket = 6,minsplit = 10,xval = 83) # since there is 6 data in Type 6
glass.treecv <- rpart(Type ~. ,data = training,method = "class",
                      control = glass.control)
printcp(glass.treecv)  # true accuracy rate for nsplit = 6: 1-53*0.64151/83 = 0.5903611
1-53*0.64151/83
# predict
predict(glass.treecv,testing,type = "class")







### subsampling ###
library(caret)
set.seed(9560)
up_train <- upSample(x = training[,-10],
                     y = training$Type)                         
table(up_train$Class) 


#
control <- trainControl(method="repeatedcv", number=10, repeats=3)

set.seed(5627)
glass.oversamp <- train(Class ~ ., data = up_train, 
                        method = "rpart",
                        trControl = control)
print(glass.oversamp)

# predict
(glasstree_uptrain.pred <- predict(glass.oversamp,testing))








### LDA ### 
# MVN test 
library(MVN)
mvn(data = training[,-10,],mvnTest = "hz")  # do not pass MVN test

# Cross Validation
library(MASS)
attach(glass)

glass.lda <- lda(Type ~. , data = training)
glass.lda
glass.ldacv <- lda(Type ~. ,data = training, CV = TRUE)
confusion.matrix <- table(real=training$Type, predict=glass.ldacv$class)
print(confusion.matrix)
accuracy.percent <- 100*sum(diag(confusion.matrix))/sum(confusion.matrix)
print(paste("accuracy:",accuracy.percent,"%"))  # CV accuracy: 59.0361445783133 %

# predict
glasslda.pred <- predict(glass.lda, testing)
glasslda.pred$class

# plot
glasslda.pred$class %>% table
eqscplot(glasslda.pred$x,type="n",xlab="1st	LD",ylab="2nd	LD")
glass.Type <- c(rep("1",3),rep("2",17),rep("3",3),rep("4",6),
                rep("5",1),rep("6",1))
glass.colors<-c(rep(1,3),rep(2,17),rep(3,3),rep(4,6),
                rep(5,1),rep(6,1))
text(glasslda.pred$x[,1:2], glass.Type, col=glass.colors)  




### using caret ###
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
glass.lda2 <- train(Type ~., method='lda', trControl = control,
                    preProcess=c('scale', 'center'), data=training)
print(glass.lda2)  # accuracy = 0.5896164021

# oversampling
set.seed(9560)
up_train <- upSample(x = training[,-10],
                     y = training$Type)                         
attach(up_train)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
oversampl.lda <- train(Class ~., method='lda', trControl = control,
                       preProcess=c('scale', 'center'), data=up_train)
print(oversampl.lda)  # accuracy = 0.7592592593

# predict 
(glasslda_uptrain.pred <- predict(oversampl.lda,testing))






### QDA ### 
# 檢測normal會不會過，用QDAtest
### test for MVN ###
training.qda <- training[,-7]
testing.qda <- testing[,-7]

# MVN test 
library(MVN)
mvn(data = training.qda[,-9,],mvnTest = "hz")  # do not pass MVN test

# findlinearcomb
findLinearCombos(glass[,-10])  # 變數之間沒有共線性問題

### Quadratic	Discriminant Analysis ###
glass.qda <- qda(Type ~. ,data=training.qda)  # some group is too small for qda, use ovesampling

# use oversampling dataset 
set.seed(9560)
up_train <- upSample(x = training.qda[,-10],
                     y = training.qda$Type)                         
table(up_train$Class) 
attach(up_train)

oversampl.qda2 <- qda(up_train$Class ~. ,data = up_train)  # rank deficiency

# findlinearcomb

findLinearCombos(glass[,-10])  # there's no linearCombs




### solve problem ###
# standardization 
x <- scale(glass[,-10])
xx <- cbind(x,glass$Type) %>% as.data.frame()
xx$V10 %<>% as.factor()

# oversampling standardized data
x <- scale(xx[,-10])
q <- cbind(x,xx[,10]) %>% as.data.frame()
q$V10%<>% as.factor()


up_train <- upSample(x = q[,-10],y = q[,10])

# MVN test 
library(MVN)
mvn(data = up_train[,-10],mvnTest = "hz")  # do not pass MVN test

### Quadratic	Discriminant Analysis ###
library(MASS)
glass.qda <- qda(up_train$Class ~. ,data=up_train)  # rank deficiency








### Nearest Neighbor Methods ###
library(class)
glass

# k = 5, apparent accuracy: 69.8795180722892 %
glass.knn5 <- knn(training[,1:9],training[,1:9],training[,"Type"],k=5,prob=T)
confusion.matrix <- table(training$Type,glass.knn5)  
# k = 4, apparent accuracy: 78.3132530120482 %
glass.knn4 <- knn(training[,1:9],training[,1:9],training[,"Type"],k=4,prob=T)
confusion.matrix <- table(training$Type,glass.knn4)
# k = 3, apparent accuracy: 83.1325301204819 %
glass.knn3 <- knn(training[,1:9],training[,1:9],training[,"Type"],k=3,prob=T)
confusion.matrix <- table(training$Type,glass.knn3)
# k = 2, apparent accuracy: 85.5421686746988 %
glass.knn2 <- knn(training[,1:9],training[,1:9],training[,"Type"],k=2,prob=T)
(confusion.matrix <- table(training$Type,glass.knn2))
# k = 1, apparent accuracy: 100 %
glass.knn1 <- knn(training[,1:9],training[,1:9],training[,"Type"],k=1,prob=T)
(confusion.matrix <- table(training$Type,glass.knn1))

# print accuracy
print(confusion.matrix)
accuracy.percent <- 100*sum(diag(confusion.matrix))/sum(confusion.matrix)
print(paste("accuracy:",accuracy.percent,"%"))

# NN-1  true error rate : 97.5903614457831 %
glass1 <- training[,c(2,3,4,8,10)]
glass.knncv <- knn.cv(glass1[,1:5],glass1[,"Type"],k=1,prob = T)
(confusion.matrix <- table(training$Type,glass.knncv))

# predict
glassknn.pred <- knn(training,testing,training[,"Type"],k=1,prob=T)
glassknn.pred





### Logistic discrimination ###
#
attach(training)
library(nnet)
glass.logd <- multinom(Type ~. , data=training,maxit=250)
glass.logd  # treated "1" as reference

cm <- table(training$Type,predict(glass.logd,training))  # misclassification rate: 
sum(diag(cm))/sum(cm)  # apparent accuracy rate: 84.34%

# true error rate by CV
library(glmnet)
x <- as.matrix(training[,-10])
y <- training$Type
glass.logdcv <- cv.glmnet(x, y, family='multinomial', type.measure='class', nfolds=83)
predict.value <- predict(glass.logdcv, x, s = "lambda.min", type = "class")
cm2 <- table(predict.value,training$Type)  # true error rate by LOOCV: 
sum(diag(cm2))/sum(cm2)  # true error rate by LOOCV: 78.31%

predict(glass.logd,testing)










##### Q4 glass  #####

### bagging ###
glass
glass.control <- rpart.control(minbucket = 6,minsplit = 10)

# install.packages('adabag')
library(adabag)
attach(training)
# the same tree with nsplit=8 was used before #
glass.bag <- bagging(Type~. , data=training,control=glass.control)
# the default number of iteration is 100 #
glassbag.pred <- predict.bagging(glass.bag,training)
glassbag.pred$confusion
glassbag.pred$error  # apparent error rate :0.1084337

glassbag.cv <- bagging.cv(Type ~. , data=training,v=83,mfinal=10,control=glass.control)  # mfinal = 20次148CV bootsrap 次數
glassbag.cv[-1]



### Boosting ###
glass.control <- rpart.control(minbucket = 6,minsplit = 10) # since there is 6 data in Type 6

glass.adaboost <- boosting(Type~., data=training, boos=F, mfinal=20,control=glass.control)  # 做20不 # boos=F 不做重新抽樣 # without bootstrap
glass.adaboost.pred <- predict.boosting(glass.adaboost,training)
glass.adaboost.pred$confusion  # apparent error rate

glass.adaboost <- boosting(Type~., data=training, boos=T, mfinal=20)
glass.adaboost.pred <- predict.boosting(glass.adaboost,training)
glass.adaboost.pred$confusion  # apparent 

glass.adaboostcv <- boosting.cv(Type~., data=training,v=83,boos=F,mfinal=10,control=glass.control)
glass.adaboostcv[-1]

glass.adaboostcv<-boosting.cv(Type~., data=training,v=83,boos=T,mfinal=10,control=glass.control)
glass.adaboostcv[-1]

newfish.adaboostcv<-boosting.cv(Species~.,data=newfish,coeflearn='Zhu',v=148,mfinal=20,control=fish.control)
newfish.adaboostcv[-1]



### randomforest ###
# model fitting
library(randomForest)
glass.rf1 <- randomForest(training$Type ~ . ,data = training, importance=TRUE,
                    ntree=2000)
glass.rfmtry4 <- randomForest(training$Type ~ . ,data = training, importance=TRUE,
                              ntree=2000,mtry=4)
plot(glass.rf1)
print(glass.rf1)
print(glass.rfmtry4)


# confusionMatrix
pred <- predict(glass.rf1, testing)
confusionMatrix(data = pred, reference = testing$Type)  # Accuracy : 0.6452 

# importance of variables
varImpPlot(glass.rfmtry5)



### Random Forest-2 ###
### random forest parameter select ###
library(randomForest)
library(mlbench)
library(caret)

# Load Dataset
glass %>% str
x <- glass[,1:9]
y <- glass[,10]

# Create model with default paramters/ reapeated cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
seed <- 100
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
glass.rf2 <- train(Type ~. , data = training, method="rf", 
                    metric=metric, tuneGrid=tunegrid, trControl=control)
glassuptrain.rf <- train(Class ~. , data = up_train, method="rf", 
                         metric=metric, tuneGrid=tunegrid, trControl=control)
print(glass.rf2)  # mtry = 3 ,acc. =  0.7507671958
print(glassuptrain.rf)  #
predict(glassuptrain.rf,testing)
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
glass.rf3 <- train(Type ~. , data = training, method="rf", 
                   metric=metric, tuneLength=15, trControl=control)
print(glass.rf3)  
plot(glass.rf3)

# Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:10))
glass.rf4 <- train(Type ~. , data=training, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(glass.rf4) 
plot(glass.rf4)





##### Q5 new_wages #####

library(caret)
set.seed(500)
inTrain <- createDataPartition(y=new_wages$Wage,p=0.7, list=FALSE)
training.1 <- new_wages[inTrain,]
testing.1 <- new_wages[-inTrain,]
attach(new_wages)
### dataset inspect & plot ###
new_wages %>% str()
colnames(new_wages)
new_wages %>% count(Wage)
table(new_wages$Wage)
table(new_wages$Education)  #
table(new_wages$South)  #
table(new_wages$Gender)  #
table(new_wages$Experience) #
table(new_wages$Union)  #
table(new_wages$Age)  #
table(new_wages$Race)  #
table(new_wages$Occupation)  #
table(new_wages$Sector)  #
table(new_wages$Mstat)  #
coolr.4 <- c("turquoise", "slategray2", "azure3", "darkseagreen1")
color.3 <- c("turquoise", "slategray2", "azure3")  
color.2 <- c("turquoise", "slategray2")  

# South
gplot.1 <- new_wages %>% count(Wage, South) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = South)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Education
gplot.1 <- new_wages %>% count(Wage, Education) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Education)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Gender
gplot.1 <- new_wages %>% count(Wage, Gender) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Gender)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Experience
gplot.1 <- new_wages %>% count(Wage, Experience) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Experience)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Union
gplot.1 <- new_wages %>% count(Wage, Union) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Union)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Age
gplot.1 <- new_wages %>% count(Wage, Age) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Age)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Race
gplot.1 <- new_wages %>% count(Wage, Race) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Race)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Occupation
gplot.1 <- new_wages %>% count(Wage, Occupation) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Occupation)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Sector
gplot.1 <- new_wages %>% count(Wage, Sector) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Sector)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 
# Mstat
gplot.1 <- new_wages %>% count(Wage, Mstat) %>% 
  ungroup() %>%    # drop if you want percentages per cylinder
  mutate(pct = prop.table(n) %>% round(3) ) 
attach(gplot.1)

ggplot(gplot.1, aes(x=Wage, y=pct, fill = Mstat)) +
  geom_bar(position = "fill", stat = "identity",color='black',width=0.9) +
  scale_y_continuous(labels = scales::percent) 






### Classification Tree ### 
# caret
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
nw.carettree <- train(Wage ~ .,method="rpart",data=training.1, 
                         trControl = control)

print(nw.carettree)  # true accuracy rate: 0.4842131605, cp=0.0243902439
plot(nw.carettree$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(nw.carettree$finalModel, use.n=TRUE, all=TRUE, cex=.8)



# rpart
library(rpart)
library(rpart.plot) 
nw.treeorig <- rpart(Wage ~. ,data = training.1,method = "class")
prp(nw.treeorig,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # 2:number of correct classifications / number of observations in that node
    extra=2)  

printcp(nw.treeorig)  # apparent error : 29.17%
plotcp(nw.treeorig)

# prune the tree : min xerror is nsplit = 9
pfit<- prune(nw.treeorig, cp=   nw.treeorig$cptable[which.min(nw.treeorig$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
prp(pfit,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # 2:number of correct classifications / number of observations in that node
    extra=2)  
# LOOCV 
nw.control <- rpart.control(minbucket = 6,minsplit = 10,xval = 376) # since there is 6 data in Type 6
nw.treecv <- rpart(Wage ~. ,data = training.1,method = "class",
                      control = nw.control)
printcp(nw.treecv)  # true error rate for nsplit = 7: 205*0.88292683/376 = 48.14%






### Random Forest ###

# model fitting
library(randomForest)
nw.rf1 <- randomForest(training.1$Wage ~ . ,data = training.1, importance=TRUE,
                          ntree=2000)
plot(nw.rf1)
print(nw.rf1)  # OOB estimate of  error rate: 51.06%

# confusionMatrix
pred <- predict(nw.rf1, testing.1)
confusionMatrix(data = pred, reference = testing.1$Wage)  # testing set Accuracy : 0.5 

# importance of variables
varImpPlot(nw.rf1)



### Random Forest-2 ###
### random forest parameter select ###
library(randomForest)
library(mlbench)
library(caret)

# Load Dataset
new_wages %>% str
x <- new_wages[,2:11]
y <- new_wages[,1]

# Create model with default paramters/ reapeated cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
seed <- 100
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
nw.rf2 <- train(Wage ~. , data = training.1, method="rf", 
                   metric=metric, tuneGrid=tunegrid, trControl=control)

print(nw.rf2)  # mtry = 3.16227766 ,acc. =  0.4991092307

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
nw.rf3 <- train(Wage ~. , data = training.1, method="rf", 
                   metric=metric, tuneLength=15, trControl=control)
print(nw.rf3)  # best mtry = 2 , acc. = 0.5146950594
plot(nw.rf3)

# Grid Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:10))
nw.rf4 <- train(Wage ~. , data=training.1, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

print(nw.rf4)  # best mtry = 2 , acc. = 0.5155985945
plot(nw.rf4)







### Xgboost ###
set.seed(500)
inTrain <- createDataPartition(y=new_wages$Wage,p=0.7, list=FALSE)
training.1 <- new_wages[inTrain,]
testing.1 <- new_wages[-inTrain,]
attach(new_wages)
new_wages$Wage %>% table()
training.1$Wage %>% table

### XGboost ### 
library(xgboost)

# Use sparse matrix in Matrix package
library(Matrix)

# transfer into sparse matrix
xdata = sparse.model.matrix(training.1$Wage ~ .-1, data = training.1)
head(xdata)
# number of categories in response variable
m = nlevels(training.1$Wage)

# transfer category into 0,1,2...
Y = as.integer(training.1$Wage) - 1

# set random seed
set.seed(12345)

# xgboost parameters setup
param = list("objective" = "multi:softprob",
             "eval_metric" = "merror",
             "num_class" = m
)

# build the model CV true error rate: 51.07%
result = xgboost(param=param, data=xdata, label=Y, nrounds=20)
result.cv <- xgb.cv(nfold = 10,param=param, data=xdata, label=Y, nrounds=20)

# (get prediction)
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))

# colnames(Ypred) = levels(iris2$Species)
Ypred = levels(training.1$Wage)[max.col(Ypred)]

# (confusion matrix)
t0 = table(training.1$Wage,Ypred)
t0

# (accuracy)
sum(diag(t0))/sum(t0)

# (variable importance) 
imp = xgb.importance(feature_names = colnames(xdata),model=result)
print(imp)

library(Ckmeans.1d.dp)
xgb.plot.importance(imp)
library(DiagrammeR)
xgb.plot.tree(feature_names = colnames(xdata),model=result, trees=2)



