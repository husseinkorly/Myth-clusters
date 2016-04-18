##########################################
# Find a(i) the disimalarity of point i  #
# with all points withen the same cluster#
##########################################
a <- function(index, data, kmeans) {
  norm <- 1 / kmeans$sizes[kmeans$clusterID[index]]
  result = matrix(nrow=nrow(data), ncol=nrow(data))
  distance <- 0
  for (i in 1:(nrow(data))) {
    if (kmeans$clusterID[i] == kmeans$clusterID[index]) {
      distance <- distance + sqrt(sum( (data[index,] - data[i,])^2 ))
    }
  }
  return(distance * norm)
}

############################################
#Find b(i) the disimalarity of point i with# 
#points of the closest cluster neighbor    #       
############################################
b <- function(index_o,data,kmeans){ 
  min_ = 9999999
  for (i in 1:kmeans$numIter+1){
    o_cluster = kmeans$clusterID[index_o]
    current_cluster_size = kmeans$sizes[i] 
    dist_sum = 0

    if(i != o_cluster) {
      for(j in 1:nrow(data)) {
        if(i == kmeans$clusterID[j]){  
          dist_sum = dist_sum + sqrt(sum((data[index_o,] - data[j,])^2)) 
        }
      }
      normalized_dist_sum = dist_sum /current_cluster_size
      if(normalized_dist_sum < min_){
        min_ = normalized_dist_sum
      }
    }
  }
  return(min_)
}

##########################################
# Find s(i). The average width of a 
# silloute for a given point i.
##########################################
s <- function(index, data, kmeans) {
  if (a(index, data, kmeans) == 0) {
    return(0)
  }
  else {
    a_o <- a(index, data, kmeans)
    b_o <- b(index, data, kmeans)
    return( (b_o - a_o) / (max(a_o, b_o) ) )
  }
}

##############################################
# Find the sum of silloutes for a given      #
# kmeans. Returns a value of -1 <= sum_s =< 1#
##############################################
calcSilhouetteCoef <- function(data,kmeans) {
  sum_s = 0
  for (i in 1:kmeans$numIter+1){
    current_cluster_size = kmeans$sizes[i] 
    for(j in 1:nrow(data)){
      if(i == kmeans$clusterID[j]) {
        sum_s = sum_s +  s(j,data,kmeans) 
      }
    }
    sum_s = sum_s/current_cluster_size
  }
  sum_s = sum_s/kmeans$numIter
  
  return(sum_s) 
}

##########################################
# Silloute method for finding best k in 
# k-means.
# dat - data to analyze
# max_k - maximum number of clusters possible
# B - number of sample sets to average
##########################################
silStatistic <- function(dat, max_k = 10,B=10){
	# normalizes data
  dat <- normalize(dat)
  opt <- -9999999
  optk <- 0
  for (k in 1:max_k) {
    clustering <- k_means(dat, k)
    temp <- calcSilhouetteCoef(dat,clustering)

    if(opt < temp) {
      opt <- temp
      optk <- k
    }
  }
  return(k)
}
