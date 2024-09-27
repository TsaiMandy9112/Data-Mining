#K-Means
iris2=iris[,-5]
set.seed(1234)
##設定隨機種子，若沒設的話每次分群會隨機再分配
kmeans.result=kmeans(iris2,3)
kmeans.result
table(iris$Species,kmeans.result$cluster)
plot(iris2,col=kmeans.result$cluster)
kmeansoutput<-cbind(iris, cluster = kmeans.result$cluster) #rbind/cbind 合併資料
View(kmeansoutput)

#K-Medoids找原生中心點
##比較不會受到極值影響
iris2=iris[,-5]
install.packages('cluster')
library(cluster)
pam.result=pam(iris2,3) ##分三群
pam.result
table(iris$Species,pam.result$clustering)
layout(matrix(c(1,2),1,2))  #圖切成兩塊
plot(pam.result)
layout(matrix(1))
Medoidsoutput<-cbind(iris, cluster = pam.result$cluster) #rbind/cbind 合併資料
View(Medoidsoutput)

#K-Prototypes
install.packages('clustMixType')
library(clustMixType)
kpres <- kproto(iris, 3, lambda = 0.1) #lambda越大類別型態加重
clprofiles(kpres, iris)
plot(iris2,col=kpres$cluster)
Prototypesoutput<-cbind(iris, cluster = kpres$cluster) #rbind/cbind 合併資料
View(Prototypesoutput)

#K的數量
install.packages('NbClust')
library(NbClust)
iris2=iris[,-5]
result=NbClust(iris2,distance="euclidean",min.nc=2,max.nc=6,method="kmeans", index="all")
result$Best.partition

#手肘法
#應用在 K-Means
install.packages('factoextra')
library(factoextra)
fviz_nbclust(data, 
             FUNcluster = kmeans,
             method = "wss", #相當於SSE  
             k.max = 12          
) +
  
  labs(title="Elbow Method for K-Means") +
  
  geom_vline(xintercept = 3,      
             linetype = 2)      

# 應用在 K-Medoid
fviz_nbclust(data, 
             FUNcluster = pam,   
             method = "wss",     
             k.max = 12          
) +
  
  labs(title="Elbow Method for K-Medoid") +
  
  geom_vline(xintercept = 3,       
             linetype = 2)         

#輪廓係數
#應用在 K-Means
fviz_nbclust(data, 
             FUNcluster = kmeans,   
             method = "silhouette", 
             k.max = 12             
) +
  
  labs(title="Avg.Silhouette Method for K-Means") 

#階層式集群Hierarchical
iris2=iris[,-5]
index=sample(1:nrow(iris2),40) 
irissample=iris2[index,]
hclust.result=hclust(dist(irissample),method= 'ward.D2') #華德法
hclust.result
plot(hclust.result,labels=iris$Species[index])
rect.hclust(hclust.result,k=3,border="red")
groups=cutree(hclust.result,k=3)
table(iris$Species[index],groups)

#密度基礎集群dbscan #可以找到Outlier在什麼位置
iris2=iris[,-5]
library(fpc)
dbscan.result=dbscan(iris2,eps=0.42,MinPts=5)
dbscan.result
table(iris$Species, dbscan.result$cluster)
plot(dbscan.result,iris2)
plot(dbscan.result,iris2[c(1,4)])
plotcluster(iris2,dbscan.result$cluster)
dbscan.result$cluster
dbscanoutput<-cbind(iris, cluster = dbscan.result$cluster) 
dbscanoutput

#outlier
kmeans.result$centers
centers=kmeans.result$centers[kmeans.result$cluster,]
head(centers)
distances=sqrt(rowSums((iris2-centers)^2))
outliers=order(distances,decreasing=T)[1:5]
outliers
iris2[outliers,]
plot(iris2[c("Sepal.Length", "Sepal.Width")], col=kmeans.result$cluster)
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=4, cex=2)
points(iris2[outliers,c("Sepal.Length", "Sepal.Width")], col=4, pch='+', cex=2)
