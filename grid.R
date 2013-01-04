## NOTE: makeGrid(0,1,10) is the same seq(0,1,len=11)
makeGrid <- function(left,right,m) { d <- m/(right-left); ((left*d):(right*d))/d }
linspace <- makeGrid

makeExpoGrid <- function(left, right, m){
  exp(makeGrid(log(left), log(right),m))
}
