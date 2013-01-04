##turn a string into a vector of length-1 strings
##"char-ize"
cz <- function(s){
  vec <- c()
  for (i in 1:nchar(s)){
    vec <- c(vec, substr(s,i,i))
  }
  vec
}

charize <- cz

##the opposite of 'cz'.
concat <- function(charVector){ Reduce(jPaste,charVector) }


trim <- function(s){
  s <- sub(' +$', '', s)
  sub('^ +','',s)
}




## EDIT DISTANCE


## INPUT: char vectors s,t
## OUTPUT: edit distance
levenshteinDist <- function(s,t){

  m <- length(s)+1
  n <- length(t)+1
  d <- matrix(nrow=m,ncol=n)

  for (i in 1:m)
    d[i,1] <- i-1
  
  for (j in 1:n)
    d[1,j] <- j-1
  
  for (j in 2:n){
    for (i in 2:m){
      ##inspect(c(i,j))
      ##inspect(c(s[i-1], t[j-1])) ##
      if (s[i-1]==t[j-1]){
        ##print("same")
        d[i,j] <- d[i-1,j-1]
      } else{
        ##print("different")
        d[i,j] <- min(d[i-1,j]+1, d[i,j-1]+1, d[i-1,j-1]+1)
      }
      ##inspect(d)      
    }
  }
  d[m,n]
}


levenshteinDistance <- function(s,t){
  if(nchar(s)>1) s <- cz(s)
  if(nchar(t)>1) t <- cz(t)
  ##inspect(list(s,t))
  levenshteinDist(s,t)
}
