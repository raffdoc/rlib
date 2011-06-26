subsetinrange <- function(x,lim)
  x[findInterval(x,sort(lim),rightmost.closed=TRUE)==1]

parusrlim <- function(side)
  par("usr")[(1:2)+2*(((side-1) %% 2+1)-1)]

timeaxis <- function (side=1, major.interval = 7, minor.interval = 1, format = "%m/%d", 
                      offset = 0, lim = parusrlim(side), extend = c(0, unit), 
                      timeclass = "chron", units = "days", 
                      labels = TRUE, major.args = NULL, minor.args = NULL,
                      tz = Sys.getenv("TZ"), origin="1970-01-01 00:00:00") {
  ## tz only for POSIXt
  ## local var
  baseunits <- switch(units, secs = 1, mins = 60, 
                      hours = 3600, days = 86400, weeks = 7 * 86400) ## from seq.POSIXt
  ## functions
  force(tz); force(origin); force(format)
  if(timeclass=="chron") {
    changetz <- identity
    formatfn <- function(x) 
      base::format(as.POSIXct(x*86400., tz = "GMT", origin=origin),
                   format = format)
    unit <- baseunits/86400.
  } else if( timeclass=="POSIXct" ) {
    changetz <- function(x)
      as.POSIXct(x, tz=tz, origin=origin)
    formatfn <- function(x)
      base::format(major.at,format=format)
    unit <- baseunits
  }
  major.interval <- major.interval*unit
  minor.interval <- minor.interval*unit
  offset <- offset*unit
  lim <- unclass(changetz(lim))  
  major.pos <- do.call(seq, c(as.list(lim + extend),by = major.interval)) + offset
  major.at <- subsetinrange(changetz(unit * (major.pos %/% unit)),lim)
  major.labels <- if(labels) formatfn(major.pos) else labels
  do.call(axis,c(list(side=side, at=major.at, labels=major.labels), major.args))
  tickpos <- list(major=major.at)
  if (!is.null(minor.interval)) {
    minor.pos <- do.call(seq, c(as.list(lim + extend), list(by = minor.interval)))
    minor.at <- changetz(unit * (minor.pos %/% unit))
    if( is.null(minor.args$tck) ) minor.args$tck <- -0.01
    do.call(axis,c(list(side=side, at=minor.at, labels=FALSE),minor.args))
    tickpos$minor <- minor.at
  }
  invisible(tickpos)
}

logaxis <- function(side, minor=TRUE, decades.only=TRUE,
                    major.labels=TRUE, minor.labels=FALSE,
                    major.args=NULL, minor.args=NULL) {
  logtest <- par(c("xlog","ylog")[(side-1) %% 2+1])
  if( !logtest )
    stop("not log scale")
  vals <- axTicks(side)
  tens <- floor(log10(vals))
  at <- (if( decades.only ) 10^unique(tens) else
         with(list(units=round(vals/(10^tens))),
              unique(units*10^tens)))
  labels <-
    (if( decades.only ) parse(text=sprintf("10^%d",log10(at))) else
     with(list(u=at,d=floor(log10(at))),
          parse(text=sprintf("%d %%*%% 10^%d",round(u/(10^d)),d))))
  do.call(axis,c(list(side=side,at=at,labels=labels),major.args))
  if( minor ) {
    tens <- do.call(`:`,as.list(range(round(log10(vals))) + c(-1,1)))
    minor.at <- outer(1:9,10^tens)
    if( is.null(minor.args$tck) ) minor.args$tck <- -0.01
    do.call(axis,c(list(side=side,
                        at=subsetinrange(minor.at,10^parusrlim(side)),
                        labels=FALSE),minor.args))
    if (minor.labels) {
      at.minor <- subsetinrange(at.minor[-1,],10^parusrlim(side))
      lab.minor <- substring(formatC(at.igor,digits=1,format="e"),1,1)
      mtext(at.igor.lab,side=1,at=at.igor,line=0,cex=.6)
    }
  }
  invisible(list(at=at,labels=labels))
}
