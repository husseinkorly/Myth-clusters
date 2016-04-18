"jump" <-
function(data=NULL,K=10,y=NULL,rand=10) {
  if (!is.null(data)) {
    fits <- kmeans.rndstart(data,K,rand)
    if (is.null(y))
    y <- dim(data)[2]/2
    n <- nrow(data)
    p <- ncol(data)
    # Compute the distortion associated with the kmeans fit
    dist<- fits/(n*p)
}
  # Call the compute.jump function to produce plots and calculate
  # maximum jump for each value of Y
  jump.results <- compute.jump(dist,y)
  jump.results$fits <- fits
}

"compute.jump" <-
function(dist,y, printresults=T) {
  K <- length(dist)
  numb.y <- length(y)
  numbclust <- rep(0,numb.y)
  transdist <- matrix(0,numb.y,K+1)
  jumps <- matrix(0,numb.y,K)
  for (i in 1:numb.y) {
    transdist[i,] <- c(0,dist^(-y[i]))
    # Compute the jumps in transformed distortion
    jumps[i,] <- diff(transdist[i,])
    # Compute the maximum jump
    numbclust[i] <- order(-jumps[i,])[1]
    print(paste("The maximum jump occurred at ",numbclust[i], "clusters with Y=",y[i]))
  }
list(maxjump=numbclust,dist=dist,transdist=transdist[,-1],jumps=jumps)}

"kmeans.rndstart" <-
function(x, K, rand = 10) {
  fits <- sum((t(x) - apply(x, 2, mean))^2)
  iter.max <- 10
  # Run kmeans for 2 to K clusters
  for (k in 2:K){
  # Run kmeans for rand inital configs 
	for (i in 1:rand) {
    Z=kmeans(x,k)
  }
  fits=c(fits,sum(Z$withinss))
  }
  fits
}
