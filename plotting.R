## if this is true, you can call 'points', otherwise need to call 'plot'
isPlotOpen <- function() (dev.cur()!=1)

pplot <- function(...) if (isPlotOpen()) points(...) else plot(...)


#######################################################################
## Scatter plot with confidence interval for the correlation + p-value
#######################################################################

## automatically plots and shows correlation, with correct axis labels
plotAndCorTest <- function(V1,V2, corTestMethod="pearson", xlab=NA, ylab=NA, showFit=NULL, fitColor="red", ...){
  stem <- function(s) sub("data\\$","",s)

  if (is.na(xlab)) xlab <- stem(deparse(substitute(V1)))
  if (is.na(ylab)) ylab <- stem(deparse(substitute(V2)))  
  
  plot(V1,V2, xlab=xlab, ylab=ylab, ...)
  ct <- cor.test(V1,V2, method=corTestMethod)
  ci <- Reduce(function(a,b) jPaste(a,",",b), format(ct$conf.int, digits=3))
  rhoString <- jPaste("rho = ", format(ct$estimate, digits=3))
  pvalString <- jPaste("p-value = ",form(ct$p.value))
  ciString <- jPaste("CI = [", ci, "]")
  title(jPaste(rhoString,"   ", ciString,"\n"))

  if(showFit=="linear") lines(V1, lm(V2 ~ V1)$fitted.values, col=fitColor)
  if(showFit=="lowess") lines(lowess(V1, V2), col=fitColor)
}



###################
## Color functions
###################

colorBackground <- function(bgColor) {
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=bgColor)
}


standardize <- function(x){ ##transform so that range is between 0 and 1
  (x-min(x))/(max(x)-min(x))
}
makeColorVector <- function(colorFn, data){
  colorFn(standardize(data))
}


library(RColorBrewer)

makeColorFn <- function(paletteName){
  jCols <- brewer.pal(n = 3, name = paletteName)
  gColor <- colorRamp(rev(jCols), bias = 1, space = c("rgb"), interpolate = c("linear", "spline"))
  function(x) {
    epsilon <- 1e-10 ##centering to take care of silly numerical problems
    x <- (1 - epsilon) * x + epsilon * 0.5
  rgb(gColor(x)/256)}
}

EPSILON <- 1e-10

roundColorIntensity <- function(x,n) floor((n+1)*(max(0,x-EPSILON)))/n
sapply(makeGrid(0,1,100), function(x) roundColorIntensity(x,5))

## produces visible edges between regions, by rounding
makeDiscreteColorFn <- function(paletteName, n){
  colFn <- makeColorFn(paletteName)
  function(x) colFn(roundColorIntensity(x))
}

## currently without interpolation
makeColorFnFromRamp <- function(colors){
  v <- colorRampPalette(colors)(256)
  function(x) v[floor((256-EPSILON)*x)+1]
}

## The following code is fully functional! (R version 2.12.2)
## It is only commented out because when this file is loaded at startup, R hasn't loaded the necessary libraries yet.  -Gustavo Lacerda, 4 Jan 2013
##
## ## the provided Red-Blue scale
## colRedBlue <- makeColorFn("RdBu")
##
## ## the provided Greyscale
## colGreys <- makeColorFn("Greys")
## the provided greyscale doesn't reach 'white' or 'black'.
## colGreys(0)
## colGreys(1)
##
## ## This solves it:
## greys <- makeColorFnFromRamp(c("white","black"))
## greys(0)
## greys(1)




##################################################
## 'qFun' is an axis-reversible version of 'Fun'.
##################################################

qplot <- function(x, y, ...) {
  if (REVERSE_PLOT) pplot(y,x, ...)
  else pplot(x,y, ...)
}

qpoints <- function(x, y, ...) {
  if (REVERSE_PLOT) points(y,x, ...)
  else points(x,y, ...)
}

qpolygon <- function(x, y, ...){
  inspect(REVERSE_PLOT)
  if (REVERSE_PLOT) polygon(y,x, ...)
  else polygon(x,y, ...)
}

qtext <- function(x, y, ...){
  if (REVERSE_PLOT) text(y,x,...)
  else text(x,y,...)
}

qabline <- function(h=NULL, v=NULL, ...){
  if (REVERSE_PLOT) abline(h=v, v=h, ...)
  else abline(h=h, v=v, ...)
}

qrect <- function(xleft, ybottom, xright, ytop, ...){
  if (REVERSE_PLOT) rect(ybottom, xleft, ytop, xright, ...)
  else rect(xleft, ybottom, xright, ytop, ...)
}




jPolygon <- function(x,y,...) polygon(c(min(x),x,max(x)), c(0,y,0),...)






## lines with changing color
colorfulLines <- function(x,y,col) {
  n <- length(x)
  ## length(X) == length(Y)
  for (i in 1:(n-1)){
    xi <- x[c(i,i+1)]
    yi <- y[c(i,i+1)]
    points(xi, yi, type="l", col=col[i])
  }
}

jitterPlot <- function(vec,...){
  v <- sapply(seq_along(vec), function(x) runif(1))
  plot(vec, v, asp=1, ...)
}


plotMatrix <- function(mat) {
  plot(1:5,1:5,type="n", xlim=c(-1, ncol(mat)), ylim=c(-nrow(mat),1))
  colGreys <- makeColorFn("Greys")
  for (i in 1:nrow(mat)){
    for (j in 1:ncol(mat)){
      n <- mat[i,j]
      ptsI <- rep(i, n); ptsJ <- rep(j, n)
      margin <- 0.5
      rect(i-margin,-j-margin,i+margin,-j+margin, col=colGreys(n))
      ##pplot(ptsI+0.5*runif(n)-0.25,-ptsJ-0.5*runif(n)+0.25, col="black")
    }
  }
}


########################################################################
## Violin plots: see http://gustavolacerda.livejournal.com/1162561.html
########################################################################

HALF_VIOLIN <- FALSE  ## only plot half of the violin

## PROBLEM: as.numeric("pi") returns NA
## eval(parse(text="piet"))
##
violinPlot <- function(datasets, property, labels=c("M", "T", "W", "R", "F", "Sa", "Su"),
                       horizontal=TRUE, colors=NA, pointColors="black", halfViolinOffset=0.05, jitterWidth=0.1, bgColor="grey", labelColor="black", xlim=NA, ...){
  property <- deparse(substitute(property))
  colors <- rep(colors, length(datasets)/length(colors)+1)
  pointColors <- rep(pointColors, length(datasets)/length(pointColors)+1)

  densities <- lapply(datasets, function(x) density(with2(x,property)))
  
  maxes <- sapply(densities, function(v) max(v$y))  
  width <- 2*max(maxes)
  if (HALF_VIOLIN) width <- width/2
  ylim <- c(0, length(datasets)+1)

  xmax <- max(sapply(densities, function(v) max(v$x)))
  xmin <- min(sapply(densities, function(v) min(v$x)))
  if (is.na(xlim)) xlim <- c(xmin,xmax)

  REVERSE_PLOT <<- !horizontal  ##set global variable
  
  if(REVERSE_PLOT)
    plot(0, 0, type="n", xlim=ylim, ylim=xlim, ylab=property, xlab="", ...)
  else
    plot(0, 0, type="n", xlim=xlim, ylim=ylim, xlab=property, ylab="", ...)

  colorBackground(bgColor)
  
  for(i in 1:length(datasets)){
    withExpr <- jPaste("with(datasets[[i]],",property,")")
    pts <- eval(parse(text=withExpr))
    d <- density(pts)
    ##d <- density(datasets[[i]][[property]])
    qpolygon(d$x, d$y/width + i, col=colors[i])
    if (!HALF_VIOLIN) qpolygon(d$x, -d$y/width + i, col=colors[i])
    qabline(h=i, col="grey")
    qtext(xlim[1],i, labels[i], col=labelColor)

    if (HALF_VIOLIN)
      ypts <- runif(length(pts), min=i+halfViolinOffset, max=i+halfViolinOffset+jitterWidth)
    else
      ypts <- runif(length(pts), min=i-jitterWidth/2, max=i+jitterWidth/2)
    qplot(pts, ypts, col=pointColors[i])
    ##qplot(pts, rep(i,length(pts)) + HALF_VIOLIN*halfViolinOffset*runif(length(pts))*doJitter)
  }
}

