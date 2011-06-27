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

make.image.raster <- function() {
  require(graphics)
  image.default <- graphics::image.default
  ## functions
  isuniformp <- function(x,y)
    anyDuplicated(x) + anyDuplicated(y) == 0
  expandaxis <- function(x) {
    dx <- diff(x)
    seq(min(x),max(x),min(dx))
  }
  expandimage <- function(x,y,z) {
    require(akima)
    akima::interp(rep(x,ncol(z)),rep(y,each=nrow(z)),z,
                  expandaxis(x),expandaxis(y))
  }
  ## code pieces
  testuniform <- quote(if( !isuniformp(x,y) ) {
    uniformgrid <- expandimage(x,y,z)
    x <- uniformgrid$x
    y <- uniformgrid$y
    z <- uniformgrid$z
    rm(uniformgrid)
  })
  rasterplot <- 
    quote(rasterImage(with(list(z=t(structure(col[zi+1],dim=dim(zi)))),
                           z[seq(nrow(z),1,-1),,drop=FALSE]),
                      min(x),min(y),max(x),max(y),
                      interpolate=interpolate))
  ## function body as list
  fnbody <- as.list(body(image.default))
  ## find locations to insert/replace  
  here <- new.env()
  patterns <- list(firstafter=alist(`if`,!is.matrix(z)),
                   imagecall=alist(.Internal,image(as.double(x),
                     as.double(y),as.integer(zi),col)))
  invisible(rapply(fnbody,assign.nodenumbers(patterns,here),how="list"))
  first <- with(here,1:firstafter)
  rest <- with(here,(firstafter+1):(imagecall-1))
  ## modifications
  formals(image.default)$interpolate <- TRUE
  body(image.default) <-
    as.call(c(fnbody[first],testuniform,fnbody[rest],rasterplot))
  ## return
  image.default
}
image.raster <- make.image.raster()

make.filled.contour.nolegend <- function() {
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
  body(filled.contour) <- as.call(c(head(fnbody,with(here,first-1)),
                                    insert,
                                    tail(fnbody,with(here,-(last-1)))))
  filled.contour
}
filled.contour.nolegend <- make.filled.contour.nolegend(graphics::filled.contour)


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

