###########  Information Theortic Approach for Choosing K            ########### 
################################################################################
###########  Sources :                                               ###########
###########  Jump Function Source Code	-                            ###########
###########  http://www-bcf.usc.edu/~gareth/research/jump            ###########
###########	 Jump Method Reference Document	-                        ###########
###########  http://www-bcf.usc.edu/~gareth/research/jumpdoc.pdf     ###########
###########  Jump Method Reserach -                                  ###########
###########	 http://www-bcf.usc.edu/~gareth/research/ratedist.pdf    ###########	
###########  More Reserach -                                         ###########
###########  http://www.proba.jussieu.fr/dw/lib/exe/fetch.php?media= ###########
###########	 users:fischer:on_the_number_of_groups_in_clustering.pdf ###########
################################################################################
"jumpMethod" <- function(data=NULL,K=10,y=NULL, count=10) {
## Maximum K is 10 by default, but we 
## can change this in the function.
	if (!is.null(data)){
    	## Cluster X with k clusters count times.
		## New centers are chosen every time,
		##The K-Means clustering with the lowest
		## Distortion is chosen.
		kmeansClustering <- kmeans.initialize(data,K, count)
  	}
  	## like our Sources, we'll let the User specify a value for Y
  	## and if they don't Y (The choice of transform power) be equal
  	## to p-dimensions in an n*p matrix over 2. Theortically p/2 
  	## is the optimal value.
  	if (is.null(y)) {
		## Data should arrive in a n*p Matrix.
		y <- dim(data)[2]/2
  	}	
  	n <- nrow(data)
  	p <- ncol(data)
  	# Distortion of the resulting clustering.
  	dist<- kmeansClustering/(n*p)
  
  	# Calculate the maximum jump for each value of Y.
  	maxJump <- maximumJump(dist,y)
  	maxJump$kmeansClustering <- kmeansClustering
  	maxJump
}

"kmeans.initialize" <- function(x, K, count = 10) {
	## Kmeans output for 2 to K clusters.
	kmeansClustering <- sum((t(x) - apply(x, 2, mean))^2)
	iter.max <- 10
  	## Cluster with Kmeans for 2 to K clusters.
  	for (k in 2:K){
  		## Pick new centers for Kmeans to restart with
  		## corresponding to count.
  		new = kmeans(x,k, nstart = count)
  		kmeansClustering=c(kmeansClustering,sum(new$withinss))
  	}
	kmeansClustering
}

"maximumJump" <- function(dist, y) {
    K <- length(dist)
	## Transformation power to a number.
    number.y <- length(y)
	## Cluster to a number, i.e. numbers 
	## corrsponding to each cluster between 1 and K.
    numberclust <- rep(0,number.y)
	## The Transformed Distortion of the each row using
	## that row's index corresponding to the index of y.
	## E.g. i-row = transformed distortion using i-y.
    transdistortion <- matrix(0,number.y,K+1)
	## This is the jumps between clusters
	## e.g. the first fow is the jump from
	## cluster 0 to cluster 1 and so-on.
    jumps <- matrix(0,number.y,K)
    for (i in 1:number.y) {
    	## Transformed distortion computation,
	  	## D[k] = d^(-Y) essentially.
      	transdistortion[i,] <- c(0,dist^(-y[i]))
      	## Jumps in transformed distortion computation.
      	jumps[i,] <- diff(transdistortion[i,])
      	## Maximum jump i.e. K that corresponds to the maxium
	    ## jump between clusters.
      	numberclust[i] <- order(-jumps[i,])[1]
    	print(paste("The K that had the maximum jump between clusters was at",numberclust[i], "clusters with Y=",y[i]))
    }
    list(maxjump=numberclust,dist=dist,transdistortion=transdistortion[,-1],jumps=jumps)
}
