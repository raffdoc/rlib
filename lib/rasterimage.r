make.image.raster <- function(image.default) {
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
  assign("patterns",list(firstafter=alist(`if`,!is.matrix(z)),
                         imagecall=alist(.Internal,image(as.double(x),
                           as.double(y),as.integer(zi),col))),,here)
  assignvars <- function(env) function(x) {
    tree <- as.list(substitute(x))
    counter <- if(exists("counter",,env)) get("counter",,env) else 1
    for( var in names(get("patterns",,env)) )
      if(!exists(var,,env) &&
         identical(tree[1:2],get("patterns",,env)[[var]]))
        assign(var,counter,,env)
    assign("counter",counter+1,,env)
  }
  invisible(rapply(fnbody,assignvars(here),how="list"))
  first <- with(here,1:firstafter)
  rest <- with(here,(firstafter+1):(imagecall-1))
  ## modifications
  formals(image.default)$interpolate <- TRUE
  body(image.default) <-
    as.call(c(fnbody[first],testuniform,fnbody[rest],rasterplot))
  ## return
  image.default
}
image.raster <- make.image.raster(graphics::image.default)
