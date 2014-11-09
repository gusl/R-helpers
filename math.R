###########
## NORMS ##
###########
L22 <- function(v) sum(v^2)
L2 <- function(v) sqrt(sum(v^2))
L1 <- function(v) sum(abs(v))

sqfrob <- function(M) L22(M)

normalize.L2 <- function(v) v/L2(v)

normalize <- function(v) v/sum(v)

##################
## LOGICAL      ##
##################

vectorEquals <- function(v1,v2) {
  ifelse(length(v1)==1, v1==v2, all(v1==v2))
}



##########################
## STOCHASTIC PROCESSES ##
##########################

matpower <- function(M,n){
  mat <- diag(nrow(M))
  for (i in 1:n){
    mat <- M %*% mat
  }
  mat
}

prinLeftEigenvector <- function(M) eigen(t(M))$vectors[,1]


isStochastic <- function(mat) vectorEquals(apply(mat,1,sum), rep(1,nrow(mat)))


##
makeAbsorbing <- function(mat, indices){
  n <- nrow(mat)
  for (index in indices){
    mat[index,index] <- 1
    mat[index,-index] <- rep(0,n-1)
  }
  mat
}

## permute rows and columns
rowColPermute <- function(mat, perm) {
  mat[perm,] <- mat
  row.names(mat) <- row.names(mat)[perm]
  inspect(mat)
  mat[,perm] <- mat
  colnames(mat) <- colnames(mat)[perm]
  inspect(mat)
  mat
}


##########################
## GEOMETRY ##############
##########################

rotationMatrix <- function(theta){ matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)))}


########################
## SET THEORY ##########
########################

##returns the first set of the partition
all2Partitions <- function(s){
  n <- length(s)
  if(n<2) return(list())
  if(n==2) return(list(s[1]))

  l <- list()
  for(i in 1:floor((n-1)/2)){
    sets <- columns(combn(s,i))
    l <- c(l,sets)
  }
  ##if even, add only half of the n/2 subsets, namely the ones in which the first elt is included
  if (n%%2==0){
    rest <- s[2:n]
    sets <- lapply(columns(combn(rest,(n-1)/2)), function(x){c(s[1], x)})
    l <- c(l,sets)
  }
  l
}

incrementBinary <- function(v){
  n <- length(v)
  i <- n
  while(i>0){
    if(v[i]==0) {
      v[i] <- 1
      break;hwhw
    } else {
      v[i] <- 0
      i <- i-1
    }
  }
  v
}

convertBinary <- function(n, N=ceiling(log2(n+1))){
  v <- rep(0,N)
  rem <- n
  for(i in 1:N){
    powerOf2 <- 2^(N-i)
    if(rem>=powerOf2){
      v[i] <- 1
      rem <- rem-powerOf2
    }
  }
  v
}

## excludes the empty set
powerSet <- function(v){
  N <- length(v)
  S <- list()
  binaryVector <- rep(0, N)
  for (i in 1:(2^N-1)){
    binaryVector <- rev(convertBinary(i,N=N))
    inspect(binaryVector)
    S[[i]] <- v[which(binaryVector==1)]
  }
  S
}


