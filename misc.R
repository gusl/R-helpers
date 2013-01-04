## this doesn't exactly change the default... afterall, defaults can be overridden
jCat <- function(...) { cat(...,sep="",fill=TRUE) }
jPaste <- function(...) {paste(..., sep="")}
jSeq <- function(a,b) if (a<=b) return(seq(a,b)) else return(NULL)


##output: list of rows
rows <- function(mat) {
  l <- list()
  for(i in 1:nrow(mat)){
    l[[i]] <- mat[i,]
  }
  l
}
## Use  as.list(data.frame(combn(1:5,2))) instead. (NOT TESTED)


##output: list of columns
columns <- function(mat) {
  l <- list()
  for(j in 1:ncol(mat)){
    l[[j]] <- mat[,j]
  }
  l
}


##unlike 'match', returns multiple matches
find <- function(elt,l){
  seq_along(l)[l==elt]
}


##best used in combination with 'cumsum', (See also: 'rmultinom')
## findLessThan(orderedVector)


## scientific notation
form <- function(x) format(x, scientific=TRUE, digits=3)


removeNA <- function(v) select(v, not(is.na))


##returns indices
which.topk <- function(v, n){
  threshold <- sort(v, decreasing=TRUE)[n]
  which(v>=threshold)[1:n]  ## randomize this
}
