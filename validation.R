source("generatedatasets.R")
source("roughGuess.R")

getSSE <- function(data, k) {
  # get kmeans data
  clustering <- k_means(data, k)
  # number of rows in data
  n <- nrow(data)
  sse <- 0
  # for each cluster
  for(i in 1:k) {
    # indeces of objects in cluster k
    l <- which (clustering$clusterID %in% i)
    # get centroid
    centroid <- clustering$finalCent[i,]
    # distance sum
    dk <- 0
    # for each object in k
    for(j in l) {
      point <- data[j,]
      dk <- dk + dist(rbind(point,centroid),method="euclidean")
    }
    sse <- sse + dk
  }
  return(sse)
}

roughGuess_SSE <- getSSE(iris[,1:4],roughGuess(iris[,1:4]))
