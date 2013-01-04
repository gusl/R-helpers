## given the parameter values, make the p,q,r,d functions
distributionFuns <- function(family,...){ ## '...' contains the parameter-values
  fam <- deparse(substitute(family))  
  pfun <- eval(parse(text=jPaste("p",fam)))
  qfun <- eval(parse(text=jPaste("q",fam)))
  rfun <- eval(parse(text=jPaste("r",fam)))
  dfun <- eval(parse(text=jPaste("d",fam)))
  
  pFun <- function(x) pfun(x,...)
  qFun <- function(x) qfun(x,...)
  rFun <- function(x) rfun(x,...)
  dFun <- function(x) dfun(x,...)
  
  list(pfun=pFun,qfun=qFun,rfun=rFun,dfun=dFun)
}

## see http://gustavolacerda.livejournal.com/1098595.html

## distr <- distributionFuns(norm,mean=10,sd=2)
## distr$qfun(0.5) ## median, a.k.a. 0.5 quantile
## distr$pfun(9)   ## cdf at 9
## distr$rfun(5)   ## sample 5 times
## distr$dfun(10)  ## density at 10

## distr <- distributionFuns(beta,shape1=2,shape2=5)
## plot(distr$dfun); abline(h=0); abline(v=distr$qfun(0.5), lty=2)
## distr$pfun(.2)
## n <- 300; points(distr$rfun(n),rep(0,n), col="#00000022")
## distr$pfun(.5)

##returns the name of the variable passed
pasteUnquoted <- function(s1,s2){
  jPaste(deparse(substitute(s1)), deparse(substitute(s2)))
}

pasteUnquoted(Hel, lo)


## turns functions that take multiple arguments into functions that take vector-arguments.
makeUnary <- function(f) function(argv) do.call(f, as.list(argv))


## e.g. if prop1 is a proposal,  pr1 <- makeUnary(prop1)



## get property is done by data[[propertyString]]

## pass a string
with2 <- function(data, expr, ...)
  with(data, eval(parse(text=expr)), ...)



## similar to 'with'. I made this because I didn't know about 'with'.
getProperty <- function(dataset, propString){

  ##propString <- deparse(substitute(propString))
  ## tokenize string into its algebraic components
  s <- strsplit(propString, split="/|\\*|\\+|-")[[1]]

  csv <- gsub("\\+", ",+,", propString)
  csv <- gsub("\\*", ",*,", csv)
  csv <- gsub("-", ",-,", csv)
  csv <- gsub("/", ",/,", csv)
  csv <- gsub("\\(", ",(,", csv)
  csv <- gsub("\\)", ",),", csv)
  csv <- gsub("\\^", ",^,", csv)
  csv <- gsub("\\%", ",%,", csv)
  
  tokens <- strsplit(csv, split=",")[[1]]
  inspect(tokens)

  symbols <- c("/","*","+","-", "(", ")", "^", "%")
  
  replacement <- c()
  for (i in 1:length(tokens)){
    token <- trim(tokens[i])
    if (!(token %in% symbols) && is.na(as.numeric(token)) && !(token %in% ls())){ ## if not a recognized string
      t <- class(try(eval(parse(text=token)), silent=TRUE))
      if (t=="try-error"){
        replacement[i] <- jPaste(dataset,"$", token)
        next;
      }
    }
    replacement[i] <- token
  }
  expr <- Reduce(jPaste,replacement)
  inspect(expr)
  eval(parse(text=expr))
}
