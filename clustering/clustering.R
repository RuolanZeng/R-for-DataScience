require(jpeg)
require(RCurl)
url <-"http://farm3.static.flickr.com/2015/2330022820_03d0231441.jpg"
readImage <- readJPEG(getURLContent(url, binary=TRUE))
dm <- dim(readImage)

rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

plot(y ~ x, data=rgbImage, main="Fruits",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")

## -------------------- kmeans --------------------

# k=2
kColors <- 2
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ]) # center of each color
plot(y ~ x, data=rgbImage, main="Fruits",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 2 colours")

# k=4
kColors <- 4
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ]) # center of each color
plot(y ~ x, data=rgbImage, main="Fruits",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 4 colours")

# k=10
kColors <- 10
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ]) # center of each color
plot(y ~ x, data=rgbImage, main="Fruits",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 10 colours")

# k=15
kColors <- 15
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ]) # center of each color
plot(y ~ x, data=rgbImage, main="Fruits",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 15 colours")

# k=20
kColors <- 20
kMeans <- kmeans(rgbImage[, c("r.value", "g.value", "b.value")], 
                 centers = kColors)
clusterColour <- rgb(kMeans$centers[kMeans$cluster, ]) # center of each color
plot(y ~ x, data=rgbImage, main="Fruits",
     col = clusterColour, asp = 1, pch = ".",
     axes=FALSE, ylab="", 
     xlab="k-means cluster analysis of 20 colours")

# find best k-value
seed = 1234
nc = 10
wss <- (nrow(rgbImage)-1)*sum(apply(rgbImage,2,var)) 
for (i in 2:nc){
  set.seed(seed)
  wss[i] <- sum(kmeans(rgbImage, centers=i)$withinss)} 

plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

## -------------------- PCA --------------------

r <- readImage[,,1]
g <- readImage[,,2]
b <- readImage[,,3]

readImage.r.pca <- prcomp(r, center = FALSE)
readImage.g.pca <- prcomp(g, center = FALSE)
readImage.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(readImage.r.pca, readImage.g.pca, readImage.b.pca)


for (i in seq.int(3, 300, length.out = 5)) {
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:i] %*% t(j$rotation[,1:i])
  }, simplify = 'array')
  writeJPEG(pca.img, paste(round(i,0), '_components.jpg', sep = ''))
}

