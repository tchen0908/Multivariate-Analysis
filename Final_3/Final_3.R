##### Final-3 CA & MCA #####
load("C:/Users/nohah/Desktop/Sessions/NCCU_STAT/106-2/多變量分析/Final/Final_3/workspace_final3.RData")

# packages
library(dplyr)
library(ca)

# replace ED1 with ED2
new_wages.combine <- read.table("new_wages.combined.txt",header = TRUE)
with(new_wages.combine, table(Education, Wage))
new_wages.combine$Education %>% table

# Correspondence Analysis 
new_wages <- read.table("new_wages.txt", header = TRUE)
new_wages.ca <- with(new_wages.combine, table(Education, Wage))
ca <- ca(new_wages.ca, nd=2)
ca
plot(ca)

# Multiple Correspondence Analysis 
rownames(new_wages.combine) <- seq(1:nrow(new_wages.combine))
mca <- mjca(new_wages.combine, nd=2, lambda="Burt")
plot(mca)
plot(mca, what = c("all", "all"), col=c("blue","red"),main="MCA")
mca$subinertia

# JCA
jca <- mjca(new_wages, nd=2, lambda="JCA")  # add things that is not on the diagnoal
summary(jca)
plot(jca)
plot(jca, col=c("blue","red"),main="JCA")
jca$levelnames
#
0.032172/0.071550
0.018569/0.071550
# compare two method, but it shold be rotate
par(mfrow=c(1,2))
plot(jca, what = c("all", "all"), col=c("blue","red"))
plot(mca, what = c("all", "all"), col=c("blue","red"))






### ---------------------------------------------------------
# Data summary
new_wages

for (i in 1:11) {
  plot(new_wages[,i], main=colnames(new_wages)[i],
       ylab = "Count", col="steelblue", las = 2)
}
table(new_wages$Education)

# replace ED1 with ED2
new_wages.combine 

# Eigenvalues / Variances
library(FactoMineR)
library("factoextra")

res.mca <- MCA(new_wages.combine, graph = FALSE)
print(res.mca)
eig.val <- get_eigenvalue(res.mca)
new_wages.combine$Education %>% table
# the percentages of inertia explained by each MCA dimensions
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 20))

# # Biplot (THIS PLOT IS INSANE)
# fviz_mca_biplot(res.mca, 
#                 repel = TRUE, # Avoid text overlapping (slow if many point)
#                 ggtheme = theme_minimal())

# extract the results for variable categories
var <- get_mca_var(res.mca)
var

# Correlation between variables and principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# visualize only variable categories
fviz_mca_var(res.mca, col.var="black", shape.var = 15,
             repel = TRUE)

# Quality of representation of variable categories
head(var$cos2, 4)

# Color by cos2 values: quality on the factor map
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# Change the transparency by cos2 values
fviz_mca_var(res.mca, alpha.var="cos2",
             repel = TRUE,
             ggtheme = theme_minimal())

# Cos2 of variable categories on Dim.1 and Dim.2
fviz_cos2(res.mca, choice = "var", axes = 1:2)

#Contribution of variable categories to the dimensions
head(round(var$contrib,2), 4)

# top 15 variable categories contributing to the dimensions
# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)

# The most important (or, contributing) variable categories
fviz_mca_var(res.mca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal()
)

# Biplot of individuals and variable categories
fviz_mca_biplot(res.mca, repel = FALSE)
? fviz_mca_biplot
