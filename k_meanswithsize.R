library(plot3D)
library(scatterplot3d)
library(car)

library(pracma)
rm(list = ls(all = T))

############################
# current working directory
############################
X <- read.table(file = "data.csv", header = T, sep = ",")

kmeans <- function(X, k) {

  X <- as.matrix(X)
  ce <<- numeric(k)
  
  cents_values <- matrix(X[kmpp(X, k), ], ncol = ncol(X))
  iteration <- 0

  while(1) {
    if( iteration != 0) {
      X <- X[, -(ncol(X))]
    }
    
    X <- as.matrix(X)
    temp <- distmat(X, cents_values)
    
    clID <- as.matrix(apply( temp, 1, which.min))
    colnames(clID)  <- c("clID")
    X <- cbind(X, clID)
    
    updated_cents <- data.frame()

    t <- as.data.frame(X)
    for(i in 1:k) {
      updated_cents <- rbind(updated_cents , colMeans(t[t$clID==i,]))
    }
    
    colnames(updated_cents)[ncol(updated_cents)] <- "ID"
    updated_cents$ID <- NULL
    updated_cents <- as.matrix(updated_cents)
    colnames(updated_cents) <- NULL
    
    epsilon <- distmat(updated_cents, cents_values)
    
    count <- 0
    for(i in 1: k) {
      if(epsilon[i, i] > .005){
        cents_values <- updated_cents
        iteration <- iteration + 1
        break
      }
      else {
        count <- count + 1
      }
    }

    if( count == k) {
      #based on lines 90-93 from dshvets
      #https://github.com/dshvets/Machine-Learning/blob/25ca77b0c82b83a21cd9eb578239f861233d7068/K-means/kMeans.R
      clusterSizes <- vector()
      for(jj in 1:k) {
        len <- length(colMeans(t[t$clID==jj,]))
        clusterSizes <- c(clusterSizes,len)
      }
      #cat("Total number of iterations to stabilize centroids = ", iteration, "\n")
      #cat("Total number of stabilized centroids = ", count, "\n")
      return (result <- list(clusterID = clID, finalCent = cents_values, numIter = iteration,sizes = clusterSizes))
    }
  }
}

kmpp <- function(X, numclusters) {
  n <- nrow(X)
  ce[1] <<- sample(1:n, 1)
  for (i in 2:numclusters) {
    dm <- distmat(X, X[ce, ])
    pr <- apply(dm, 1, min); pr[ce] <- 0
    #show(pr)
    #show(dm)
    ce[i] <<- sample(1:n, 1, prob = pr)
  }
  return(ce)
}

plotDataAndCentroids <- function(X, cents_values, k, dimension) {

  plot(X[, -ncol(X)], main = "Cluster Distributions", sub = " kmeans clustering")
  
  start <- 15
  for(i in 1:k) {
    if( dimension <= 2) {
      points(subset(X[, -ncol(X)], X$clID == i), col = i, pch = start)
    }
    start <- start + 1
  }

  points(cents_values, col = "blue", pch =8, cex = 2)

  if(dimension == 3)
  scatterplotMatrix(~length + width + height | clID, data=X, main="clusters", smoother=FALSE, reg.line = FALSE, diagonal = "histogram", ellipse = TRUE)

}

calculateSSE <- function(X, cents_values, k) {

  clusterSSE <- data.frame()
  for(i in 1:k) {
    centroid <- as.vector(cents_values[i,])
    clusterSubset <- as.matrix(subset(X[, -ncol(X)], X$clID == i))
    sumerror <- distmat(clusterSubset, centroid)
    resultRow <- c(i, colSums(sumerror))
    clusterSSE <- rbind(resultRow, clusterSSE)
  }
  
  colnames(clusterSSE) <- c("clusterID","clusterSSE")
  SSEMeasuresTotal <- as.matrix(colSums(as.matrix(clusterSSE$clusterSSE)))
  colnames(SSEMeasuresTotal) <- c("totalSSE")
  return(result <- list(clusterSSEmeasure = clusterSSE, totalSSE = SSEMeasuresTotal))
}

plotTotalSSE <- function(kmin, kmax, X) {
  
  pointsToplot <- data.frame()
  for( i in kmin: kmax) {
    results <- kmeans(X, i)
    Xnew <- cbind(X, results$clusterID)
    pointy <- calculateSSE(Xnew, results$finalCent, i )$totalSSE
    resultRow <- c(i, pointy)
    pointsToplot <- rbind(resultRow, pointsToplot)
  }
  colnames(pointsToplot) <- c("K value", "SSE")
  plot(pointsToplot, main = "Basic Elbow Plot for k values", type = "o", col="blue")
}
