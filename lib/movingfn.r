movingwindow <- function(x,y=NULL,xrange=NULL,nbins=1000,
                         increment=diff(xrange)/nbins,
                         windowsize=increment*2,
                         FUN=function(x) length(x)/windowsize,...) {
  if(is.null(xrange)) xrange <- range(x)
  alongx <- seq(xrange[1],xrange[2],increment)
  values <- vector("numeric",length(alongx))
  for( i in seq(along=alongx) )
    values[i] <- FUN(`[`(if(!is.null(y)) y else x,
                         x >= alongx[i] - windowsize/2 &
                         x <  alongx[i] + windowsize/2),...)
  data.frame(pts=alongx,vals=values)
}
