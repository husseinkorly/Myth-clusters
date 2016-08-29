##########################################################
# generates uniformly distributed sample data
# data = actual data (used to match length and dimensions)
##########################################################
getSampleData <- function(data){
  # number of rows in actual dataset
  n <- nrow(data)
  attrs <- list()
  # length(data) is number of attributes (dimensions)
  for(i in 1:length(data)) {
    # minimum value at current attribute
    minval <- min(data[,i])
    # maximum value at current attribute
    maxval <- max(data[,i])
    # save random values from min to max
    attrs[[i]] <- runif(n, minval, maxval)
  }
  # combine all attributes
  sampledata <- data.frame(attrs)
  return(sampledata)
}

##################################################################
# get standard deviation within a cluster
# standard deviation calculation is only used for sample datasets
# to calculate SD:
# subtract mean from log(Wk) and square value for each dataset
# sum these values
# multiply by 1 / B (number of datasets)
# take the square root
# log_Wk_list = list of log(Wk) values for data
# mean = mean of log(Wk) values
# k = current k value
##################################################################
getSD <- function(log_Wk_list,mean,k){
  sd_sum <- 0
  # number of datasets
  B <- length(log_Wk_list)
  for(i in 1:B){
    sd_sum <- sd_sum + (log_Wk_list[[B]]-mean)^2
  }
  return( ((1/B)*sd_sum)^.5 )
}

############################################
# returns Wk for a given number of clusters
# Wk is a variance quantity
# Wk = sum(D_k / 2n_k)
# data = data (actual or sample)
# k = number of clusters
############################################
getWk <- function(data,k){
  # get kmeans data
  clustering <- k_means(data, k)
  # number of rows in data
  n <- nrow(data)
  Wk <- 0
  # for each cluster
  for(i in 1:k) {
    # indeces of objects in cluster k
    l <- which (clustering$clusterID %in% i)
    # get centroid
    centroid <- clustering$finalCent[i,]
    # distance sum
    dk <- 0
    # for each object in k
    for(j in l){
      point <- data[j,]
      dk <- dk + dist(rbind(point,centroid),method="euclidean")
    }
    Wk <- Wk + dk
  }
  return(Wk)
}


#############################################
# Gap Statistic method for finding best k
# in k-means.
# dat - data to analyze
# max_k - maximum number of clusters possible
# B - number of sample sets to average
#############################################
gapStatistic <- function(dat, max_k=10, B=10) {
  # normalizes data
  dat <- normalize(dat)
  
  # Create sample sets
  sampleList <- list()
  for(i in 1:B){
    sampleList[[i]] <- getSampleData(dat)
  }
  
  # Gap Statistic algorithm
  Wk_list <- c()
  # start with k = 1, test to k = max_k
  k <- 1
  repeat {
    # Wk for sample data
    log_Wk_sample_list <- c()
    # sum of log(Wk) values
    sum <- 0
    # for each sample dataset
    for(i in 1:B){
      # get Wk for sample dataset
      Wk <- getWk(sampleList[[i]],k)
      # take log of Wk
      log_Wk <- log2(Wk)
      # add log_Wk to list
      log_Wk_sample_list[[i]]<-log_Wk
      # add log_Wk to sum
      sum <- sum + log_Wk
    }
    # mean of log(Wk) values
    log_Wk_sample <- sum / B
    
    # Wk for actual data
    log_Wk_data <- log2(getWk(dat,k))
    # used for plotting
    Wk_list<-rbind(Wk_list,c(k,log_Wk_data,log_Wk_sample))
    
    # find gap value
    gap <- log_Wk_sample - log_Wk_data
    # find standard deviation
    sd <- getSD(log_Wk_sample_list, log_Wk_sample, k)
    # find sk value
    sk <- ((1 + (1/B))^0.5) * sd
    
    # check if k is too large
    if(k > max_k) {
      print("Sorry, maxed out on k.")
      break
    }
    
    # k is optimal when Gap(k) >= Gap(k + 1) - sk
    if (k != 1 && prev_gap >= (gap - sk)) {
      # optimal k is at k - 1
      k <- k - 1
      # plot Wk data
      df <- data.frame("x"=Wk_list[,1],"Wk"=Wk_list[,2],"Wk_sample"=Wk_list[,3])
      plot(df[1:2], col = "blue", pch = 20, type="o") #data
      points(df[3], col = "red", pch = 20, type="o") #sample
      break
    }
    
    prev_gap <- gap
    # increment k
    k <- k + 1
  }
  # return the optimal k value
  return(k)
}
