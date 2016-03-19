library(pracma)

rm(list = ls(all = T))
setwd("C:/Users/Hussein/repo2/Myth clusters")
X <- read.table(file = "data2.csv", header = T, sep = ",")
data <- read.table(file = "data2.csv", header = T, sep = ",")
X <- as.matrix(data)
n <- nrow(X)
C <- numeric(2)

kmpp <- function(X, k) {
  C[1] <<- sample(1:n, 1)
  for (i in 2:k) {
    dm <- distmat(X, X[C, ])
    pr <- apply(dm, 1, min); pr[C] <- 0
    C[i] <<- sample(1:n, 1, prob = pr)
  }
  return(C)
}

cents_values <- matrix(X[kmpp(X, 2), ], ncol = ncol(X))


temp <- distmat(X, cents_values)
mins <- c()
for(i in 1:nrow(X)){
  mins <- append(mins, min(which.min(temp[i,])))
}

data$cluster <- mins
