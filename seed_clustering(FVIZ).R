library(readxl)
library(cluster)
z1<-read_excel("D:\\DATA ANALYSIS\\EXCEL SHEETS\\seeds.xlsx")
names(z1)
View(z1)
colSums(is.na(z1))
######### determining number of clusters
wss<-0
for(i in 1:15){
  wss[i]<-sum(kmeans(z1,centers = i)$withinss)
}
plot(1:15,wss,type="b")
######## clustering with 3 clusters
set.seed(123)
z2<-scale(z1)
clus<-kmeans(z2,3)
clus$size
clus$withinss
clus$betweenss
####### silhouette coeff
diss<-daisy(z2)
diss1<-diss^2
silcof<-silhouette(clus$cluster,diss1)
plot(silcof)
####### plotting
library(fpc)
library(factoextra)
fviz_cluster(clus,z2)
clusplot(z2,clus$cluster,color = T,shade = T)
####### appending
z3<-data.frame(z1,clusters=clus$cluster)
write.csv(z3,"D:\\DATA ANALYSIS\\Output CSV\\seed(clustering).csv")
