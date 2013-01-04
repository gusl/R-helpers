## by Gustavo Lacerda, 4 Jan 2013
## http://www.optimizelife.com


jCat <- function(...) { cat(...,sep="",fill=TRUE) }

NO_INSPECT <- FALSE  ## set to TRUE to turn off 'inspect'
ACTIVE_TAGS <- NULL  ## adjust this to see different types/levels of detail



inspect <- function(stuff, tags=NULL){ ## if no tags specified, always print
  intersection <- intersect(ACTIVE_TAGS, tags)
  if (NO_INSPECT) return()
  if (!is.null(tags) && !is.null(ACTIVE_TAGS) && length(intersection)==0) return()
  exprString <- as.character(match.call()[2])
  if (length(stuff)==1 && !is.data.frame(stuff) && !is.function(stuff)) { ##if not a proper
    ##vector / matrix / complex object, then it can be printed with 'cat', in a single line.
    jCat(exprString, " = ", stuff); return(stuff)
}
  else { ## if complex object, must be printed with 'print'.
    jCat(exprString, " = "); print(stuff)
  }
}


## ## USAGE:
## M <- matrix(rnorm(4), nrow=2)
## x <- 5
## inspect(x)
## inspect(M)
## inspect(M+x)
## inspect(apply(M,1:2, function(x) x^2))
## inspect(M+x)^2  ##prints M+x, evaluates (M+x)^2;  NOT RECOMMENDED because this hurts readability.
## inspect(M)+x
## inspect(inspect(x)+M)  ## outer exprString will print as 'inspect(x) + M'.  Could be cleaned up.


## NO_INSPECT <- TRUE
## inspect(M+x)

## NO_INSPECT <- FALSE
## inspect(M+x)

## ACTIVE_TAGS <- c("A", "B")
## inspect(M+x, c("C","D")) ## intersection is empty: does not print.

## ACTIVE_TAGS <- c(ACTIVE_TAGS, "C")
## inspect(M+x, c("C","D")) ## now it prints.

## inspect(M+x) ## a call to 'inspect' in which tags is NULL will always print.

## ACTIVE_TAGS <- NULL
## inspect(M+x, c("C","D")) ## likewise, if ACTIVE_TAGS is NULL, it always prints.
