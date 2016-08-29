################################################
# Datasets we need: (Feel free to add or remove)
# 2-d demonstration set with 3 or 4 clusters
# 3-d dataset
# Dataset with clusters of different sizes/spreads
# Uniform dataset
# Smiley face shape (3 clusters)
#################################################

# Normalize data with function normalize
# nfunc - helper function for normalize
nfunc <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

normalize <- function(data) {
  return(as.data.frame(lapply(data, nfunc)))
}

# Generates clusters with different sizes and standard deviations
# numdim - number of dimensions
# nc - number of clusters to generate
# clustersizes - list of number of points in each cluster
# clustersds - list of standard deviations for each cluster 
gaussClusters <- function(numdim, nc, clustersizes, clustersds) {
  attrs <- list()
  #Generate each attribute (dimension) for each cluster
  
  #Initialize possibilites
  free <- list()
  free[1:numdim] <- list(1:nc)
  centroids <- (1:(nc+1)/nc)
  
  for(k in 1:nc) {
    n <- clustersizes[k]
    sd <- clustersds[k]
    
    for (i in 1:numdim) {

      start <- sample(free[[i]],1)
      centroid <- runif(1, centroids[start], centroids[start+1])
      free[[i]] <- free[[i]][free[[i]]!=start]
      show(start)
      show(free[[i]])
      
      #If attribute is empty, initialize it to centroid
      if(length(attrs) < i) { 
        attrs[[i]] <- centroid
      } 
      #If attribute already exists, add centroid to it
      else { 
        attrs[[i]] <- c(attrs[[i]],centroid)
      }
      #Generate the cluster for the centroid with a certain spread and add to attribute
      cluster <- centroid + rnorm(n-1, 0, sd)
      attrs[[i]] <- c(attrs[[i]], cluster)
    }
  }
  clusterData <- data.frame(attrs)
  return(clusterData)
}

# Generates uniform dataset within certain box of values
# numdim - number of dimensions
# n - number of points
# minimums - Minimum values of each attribute
# maximums - Maximum values of each attribute
uniform <- function(numdim, n, minimums, maximums){
  attrs <- list()
  # length(data) is number of attributes (dimensions)
  for(i in 1:numdim) {
    # save random values from min to max
    attrs[[i]] <- runif(n, minimums[i], maximums[i])
  }
  # combine all attributes
  data <- data.frame(attrs)
  return(data)
}

# Generates a smiley face with randomly spread eyes and random curve of mouth
smiley <- function() {
  min <- 0
  max <- 20
  
  # generate eye
  n <- 500
  eye_size <- runif(1, 1, ((max-min) / 6))
  eye_dist_out = runif(1, (eye_size / 2), (((max-min) / 2) - (eye_size / 2)))
  x1 <- eye_dist_out
  x2 <- max - eye_dist_out
  eye_y <- runif(1, (((max-min) / 2) + (eye_size / 2)), (max - (eye_size / 2)))
  
  rho <- sqrt(runif(n)) * eye_size / 2
  theta <- runif(n, 0, runif(1, 0, 2)*pi)
  x_vals1 <- x1 + (rho * cos(theta))
  y_vals1 <- eye_y + (rho * sin(theta))
  
  x_vals2 <- x2 + (rho * cos(theta))
  y_vals2 <- eye_y + (rho * sin(theta))

  # generate nose
  n <- 500
  nose_height <- runif(1, 1, max/3)
  nose_width <- runif(1, 1, max/3)
  v1_x <- (max - min) / 2
  v2_x <- v1_x - (nose_width / 2)
  v3_x <- v1_x + (nose_width / 2)
  v1_y <- (max - min) / 2
  v2_y <- v1_y - nose_height
  v3_y <- v1_y - nose_height
  r1 <- runif(n, 0, 1)
  r2 <- runif(n, 0, 1)
  nose_x <- ((1 - (r1 ^ 0.5)) * v1_x) + ((r1 ^ 0.5) * (1 - r2) * v2_x) + (r2 * (r1 ^ 0.5) * v3_x)
  nose_y <- ((1 - (r1 ^ 0.5)) * v1_y) + ((r1 ^ 0.5) * (1 - r2) * v2_y) + (r2 * (r1 ^ 0.5) * v3_y)

  # generate mouth
  width <- runif(1, max/4, max/2)
  flatnessfactor = runif(1, width*2, width*4)
  #lower  
  mouth_x <- runif(n, -width, width)
  mouth_y <- (mouth_x^2)/flatnessfactor
  #upper
  shift <- max(mouth_y)/2
  mouth_y <- c(mouth_y,((mouth_x^2)/(flatnessfactor*2)) + shift)
  mouth_x <- mouth_x + ((max-min)/2)
  mouth_x <- c(mouth_x,mouth_x)
  
  x <- c(x_vals1, x_vals2, nose_x, mouth_x)
  y <- c(y_vals1, y_vals2, nose_y, mouth_y)
  dat <- data.frame(x, y)
  plot(dat, pch=19, cex=0.6, col="#00000020")
  
}
