#A method found online
#https://www.quora.com/How-can-we-choose-a-good-K-for-K-means-clustering


roughGuess <- function(data) { 
  return(floor((nrow(data)/2)^0.5))

}
