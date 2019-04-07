require(jpeg)
require(RCurl)
url <-""
readImage <- readJPEG(getURLContent(url, binary=TRUE))
dm <- dim(readImage)
dm
readImage[1,2,1]

rgbImage <- data.frame(
  x=rep(1:dm[2], each=dm[1]),
  y=rep(dm[1]:1, dm[2]),
  r.value=as.vector(readImage[,,1]),
  g.value=as.vector(readImage[,,2]),
  b.value=as.vector(readImage[,,3]))

head(rgbImage)

plot(y ~ x, data=rgbImage, main="Lloyd building",
     col = rgb(rgbImage[c("r.value", "g.value", "b.value")]), 
     asp = 1, pch = ".")
