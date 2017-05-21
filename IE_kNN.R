IE <- function(X,k1){
  library(FNN)
  #Information Energy IE(X)
  d<-1#space dimensionality
  V1_1D=2#volume of the unit ball in a 1D space
  n<-length(X)
  if(missing(k1))
    k1 <- as.integer(n^(4/(d+4)))
  R <- knn.dist(X, k=k1, algorithm = "kd_tree")
  IEX_est = 0
  for(i in 1:n)
  {
    IEX_est <- IEX_est + 1/R[i,k1]
  }
  IEX_est <- IEX_est * k1 / (n * n * V1_1D)
  
  return(IEX_est)
}

IEcond <- function(X,Y,k2){
  library(FNN)
  #Conditional Information Energy IE(X|Y)
  d<-2#space dimensionality
  V1_2D=pi#volume of the unit ball in a 2D space
  n<-length(X)
  p<-length(Y)
  if(missing(k2))
    k2 <- as.integer(length(Y)^(4/(d+4)))
  data<-matrix(c(X,Y),nrow=length(Y))
  R<-knn.dist(data, k=k2, algorithm = "kd_tree")
  IEXY_est = 0
  for(i in 1:length(Y))
  {
    IEXY_est <- IEXY_est + 1/(R[i,k2])^2
  }
  IEXY_est <- IEXY_est * k2 / (n * p * V1_2D) 
  
  return(IEXY_est)
}

o <- function(X, Y){
  library(FNN)
  #o(X,Y)
  oXY_est = IEcond(X,Y)-IE(X)
  
  return(oXY_est)
}
