
###_* low-level functions
arc <- function(r,dtheta) {
  theta <- seq(0,dtheta,,100)*pi/180
  x <- r*cos(theta)
  y <- r*sin(theta)
  list(x=x,y=y)
}
slice <- function(r,dtheta) {
  with(arc(r,dtheta),list(x=c(0,x,0),y=c(0,y,0)))
}
rotate <- function(v,theta) {
  if(is.list(v)) v <- do.call(rbind,v)
  theta <- theta*pi/180
  R <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),ncol=2)
  vp <- R %*% v
  `names<-`(split(vp,row(vp)),c("x","y"))
}
translate <- function(xy,offset) Map(`+`,xy,offset)

###_* higher-level functions

pieanywhere <- function(delta_angles,colors,radius,offset,...) {
  ## (delta_angles in units of degrees)
  ## (radius in usr units of x)
  ## (offset in usr units of x,y)
  invisible(mapply(function(a,col,w,r,r0)
                   with(translate(rotate(slice(r,w),a),r0),
                        polygon(x,y,col=col,...)),
                   a=c(0,head(cumsum(delta_angles),-1)),
                   col=colors,w=delta_angles,
                   MoreArgs=list(r=radius,r0=offset)))
}
xyratio <- function(x) {
  mat <- matrix(x,ncol=2,dimnames=list(c("lower","upper"),c("x","y")))
  diff(mat[,"x"])/diff(mat[,"y"])
}

adjustpos <- function(x,y,usr,plt) {
  interplin <- function(val,x,y) y[1]+diff(y)/diff(x)*(val-x[1])
  asp <- xyratio(usr)/xyratio(plt)
  xlim <- usr[1:2]
  ylim <- usr[3:4]
  if(asp >= 1) {
    plot.window(xlim=xlim*asp,ylim=xlim,xaxs="i",yaxs="i",asp=1)
    newx <- interplin(x,xlim,par("usr")[1:2])
  } else {
    plot.window(xlim=xlim,ylim=xlim*asp,xaxs="i",yaxs="i",asp=1)
    newx <- x
  }
  newy <- interplin(y,ylim,par("usr")[3:4])
  list(x=newx,y=newy)
}

###_* main function

placepies <- function(values,x,y,size,col,...)
  UseMethod("placepies")

placepies.default <- function(values,x,y,size,col,...) {
  oldpar <- par(no.readonly=TRUE)
  newxy <- adjustpos(x,y,oldpar$usr,oldpar$plt)
  if( is.null(dim(values)) ) {
    pieanywhere(delta_angles=values/sum(values)*360,
                radius=size,
                offset=newxy,
                colors=col,...)
  } else {
    invisible({
      Map(pieanywhere,
          delta_angles=with(list(m=as.matrix(values)),
            split(sweep(m,1,rowSums(m),'/')*360,row(m))),
          radius=size,
          offset=Map(c,newxy$x,newxy$y),
          MoreArgs=c(list(colors=col),list(...)))
    })
  }
  par(oldpar)
  plot.window(oldpar$usr[1:2],oldpar$usr[3:4],xaxs="i",yaxs="i",asp=oldpar$asp)
}
