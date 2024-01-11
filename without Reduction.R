new <- read.table("C:/Users/G10/Desktop/new6.txt",sep = ",")
X<-data.matrix(new, rownames.force = NA)
scaling=TRUE
x.vlm<- NbClust(X,distance="euclidean",min.nc=2 ,max.nc =20,method = "kmeans",index ="hartigan",alphaBeale = 0.1)
x.vlm
x.km<-kmeans(X,4)
x.km$cluster
x.km
X[x.km$cluster==1,]
plot(X[,1],X[,2],type="n")
text(X[,1],X[,2],labels=as.character(x.km$cluster))
SSW<- x.km$tot.withinss
MSE <- SSW/565
fratek32 <- x.km$tot.withinss/x.km$betweenss
x.km
a<-css(dist(X),x.km$cluster)
#______________________________________________K-mean Original Data
hc<-hclust(dist(X), "average",)
HClust<-cutree(hc,k=4)
ggdendrogram(hc)
x.clm <-mean(hc$height)
x.cls <-sqrt(var(x.clfl))
x.clfl <-hc$height
print(x.clfl-x.clm)/x.cls
x.clfl[568]
x.clim <-cutree(hc,h=x.clfl[199],k=4)
plot(X[,1],X[,2],type="n")
text(X[,1],X[,2],labels=as.character(x.clim))

b<-css(dist(X),HClust)
frateH<-b$totwss/b$totbss

