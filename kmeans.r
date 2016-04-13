library(plot3D)
library(scatterplot3d)
library(car)

library(pracma)
rm(list = ls(all = T))

############################
# current working directory
############################
#setwd("C:/Users/Krishna/Desktop/CSC522_datascience/Project")
X <- read.table(file = "data.csv", header = T, sep = ",")

kmeans <- function(X, k) {

	X <- as.matrix(X)
	ce <<- numeric(k)

	cents_values <- matrix(X[kmpp(X, k), ], ncol = ncol(X))
	iteration <- 0

	while(1) {
		show(iteration)
		show("Centroids values")
		show(cents_values)

		if( iteration != 0) {
			X <- X[, -(ncol(X))]
		}

		X <- as.matrix(X)
		temp <- distmat(X, cents_values)

		clID <- as.matrix(apply( temp, 1, which.min))
		colnames(clID)  <- c("clID")
		X <- cbind(X, clID)
		show(X)

		updated_cents <- data.frame()
		t <- as.data.frame(X)
		show(is.matrix(X))

		for(i in 1:k){
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
			cat("Total number of stabilized centroids = ", count, "\n")
			return (result <- list(clusterID = clID, finalCent = cents_values, numIter = iteration))
		}
	}
}

kmpp <- function(X, numclusters) {
	n <- nrow(X)
	ce[1] <<- sample(1:n, 1)
	for (i in 2:numclusters) {
		dm <- distmat(X, X[ce, ])
		pr <- apply(dm, 1, min); pr[ce] <- 0
		ce[i] <<- sample(1:n, 1, prob = pr)
	}
	return(ce)
}

#TODO 
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
	scatterplotMatrix(~length + width + Height | clID, data=X, main="clusters", smoother=FALSE, reg.line = FALSE, diagonal = "histogram", ellipse = TRUE)
}
