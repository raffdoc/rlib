assign.nodenumbers <- function(patterns,env) function(x) {
  ## use with rapply(...,how="list")
  tree <- as.list(substitute(x))
  counter <- if(exists("counter",,env,inherits=FALSE)) get("counter",,env) else 1
  for( var in names(patterns) )
    if(!exists(var,,env,inherits=FALSE) &&
       identical(tree[1:length(patterns[[var]])],
                 patterns[[var]]))
      assign(var,counter,,env)
  assign("counter",counter+1,,env)
}

logseq <- function(base=exp(1))
  as.function(c(formals(base::seq.default),quote({
    invocation <- lapply(as.list(match.call()),eval,envir=parent.frame())
    ft <- Filter(Negate(is.null), as.list(invocation)[c("from", "to")])
    invocation[names(ft)] <- lapply(ft, function(x,b) log(x , b),base)
    base^do.call(seq, invocation[-1])
  })))
log10seq <- logseq(10)

## make.image.raster <- function() {
##   ## deprecated after Version 2.12.2
##   ## Version 2.13.1 has 'useRaster' argument
##   require(graphics)
##   image.default <- graphics::image.default
##   ## functions
##   ## code pieces
##   fndefs <-
##     alist(isuniformp <- function(x,y)
##           anyDuplicated(x) + anyDuplicated(y) == 0,
##           expandaxis <- function(x) {
##             dx <- diff(x)
##             seq(min(x),max(x),min(dx))
##           },
##           expandimage <- function(x,y,z) {
##             require(akima)
##             akima::interp(rep(x,ncol(z)),rep(y,each=nrow(z)),z,
##                           expandaxis(x),expandaxis(y))
##           })
##   testuniform <-
##     quote(if( !isuniformp(x,y) ) {
##       uniformgrid <- expandimage(x,y,z)
##       x <- uniformgrid$x
##       y <- uniformgrid$y
##       z <- uniformgrid$z
##       rm(uniformgrid)
##     })
##   rasterplot <- 
##     quote(rasterImage(with(list(z=t(structure(col[zi+1],dim=dim(zi)))),
##                            z[seq(nrow(z),1,-1),,drop=FALSE]),
##                       min(x),min(y),max(x),max(y),
##                       interpolate=interpolate))
##   ## function body as list
##   fnbody <- as.list(body(image.default))
##   ## find locations to insert/replace  
##   here <- new.env()
##   patterns <- list(firstafter=alist(`if`,!is.matrix(z)),
##                    imagecall=alist(.Internal,image(as.double(x),
##                      as.double(y),as.integer(zi),col)))
##   invisible(rapply(fnbody,assign.nodenumbers(patterns,here),how="list"))
##   first <- with(here,1:firstafter)
##   rest <- with(here,(firstafter+1):(imagecall-1))
##   ## modifications
##   formals(image.default)$interpolate <- TRUE
##   body(image.default) <-
##     as.call(c(fnbody[first],fndefs,testuniform,fnbody[rest],rasterplot))
##   ## return
##   image.default
## }
## image.raster <- make.image.raster()

make.filled.contour.free <- function() {
  require(graphics)
  filled.contour <- graphics::filled.contour
  fnbody <- as.list(body(filled.contour))
  insert <- quote(if( add ) { axes <- FALSE } else {
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  })
  here <- new.env()
  patterns <- list(first=alist(`<-`,mar.orig),
                   last=alist(`if`,!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L))
  invisible(rapply(fnbody,assign.nodenumbers(patterns,here),how="list"))
  formals(filled.contour)$add <- FALSE
  body(filled.contour) <- as.call(c(head(fnbody,with(here,first-1)),
                                    insert,
                                    tail(fnbody,with(here,-(last-1)))))
  filled.contour
}
filled.contour.free <- make.filled.contour.free()

make.newimageplot <- function() {
  require(fields)
  image.plot <- fields::image.plot
  fnbody <- as.list(body(image.plot))
  parsettings <- quote({
    par(big.par)
    par(plt = big.par$plt, xpd = TRUE)
    par(mfg = mfg.save, new = FALSE)
    invisible()
  })
  clipping <- quote(par(xpd=FALSE))
  here <- new.env()
  pattern <- list(replacement=alist(`if`,graphics.reset | add))
  invisible(rapply(fnbody,assign.nodenumbers(pattern,here),how="list"))
  fnbody[[get("replacement",,here)]] <- parsettings
  body(image.plot) <- as.call(c(fnbody,clipping))
  image.plot
}
newimageplot <- make.newimageplot()

legend.colorbar <- function(...,axes=FALSE,ann=FALSE,
                            horizontal=FALSE,
                            axis.log="",
                            axis.side=if(horizontal) 1 else 4,
                            imagefn=graphics::image.default,
                            axis.args=NULL,
                            major.axis.args=axis.args,
                            minor.axis.args=NULL) {
  ## to be called in conjunction with layout(), usually
  ## imagefn can also be image.raster() if also defined
  args <- list(...)
  nc <- length(if(is.null(args$col)) eval(formals(imagefn)$col) else args$col)
  if( !is.null(args[["zlim"]]) ) {
    ## zlim exists
    zrange <- args[["zlim"]]
  } else if( !is.null(args[["z"]]) ) {
    ## z exists
    zrange <- range(args[["z"]],na.rm=TRUE)
  } else {
    ## fail
    stop("z or zlim must be specified")
  }
  x <- 1
  y <- seq(zrange[1],zrange[2],,nc)  
  z <- 1:nc
  if( horizontal ) {
    args$x <- y
    args$y <- x
  } else {
    args$x <- x
    args$y <- y
  }
  ## args$z <- structure(z,dim=c(length(args$x),length(args$y)))
  args$z <- structure(y,dim=c(length(args$x),length(args$y)))
  do.call(imagefn,c(args,list(log="",axes=axes,ann=ann)))
  if( !is.na(axis.side) ) {
    if( axis.log=="z" ) {
      zaxisrange <- range((par("usr")[if(horizontal) 1:2 else 3:4] %/% 1) + c(-1,1))
      zticks <- zaxisrange[1]:zaxisrange[2]
      if(is.null(major.axis.args$tck))
        major.axis.args$tck <- 
          (if(is.na(par("tck"))) par("tcl") else par("tck"))
      if(is.null(minor.axis.args$tck))
        minor.axis.args$tck <- 1/5*major.axis.args$tck
      do.call(axis,c(list(side=axis.side,at=zticks,labels=
                          parse(text=sprintf("10^%d",zticks)),
                          xpd=FALSE),major.axis.args))
      do.call(axis,c(list(side=axis.side,at=log10(outer(1:9,10^zticks)),
                          labels=FALSE,xpd=FALSE),minor.axis.args))
    } else {
      do.call(axis,c(list(side=axis.side),axis.args))
    }
    box()
  }
  invisible()
}
attr(legend.colorbar,"example") <- "
layout(t(1:2),width=c(5,1))
z <- runif(100)
plot(z,col=heat.colors(100))
legend.colorbar(z=z,col=heat.colors(100))
"
