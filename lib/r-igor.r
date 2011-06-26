makescale <- function(x,a) {
  f <- (if( !is.null(dim(x)) )
        function(x) dim(x)[match(a,c("x","y","z","t"))] else
        length)
  seq(attr(x,a)$start,,attr(x,a)$inc,f(x))
}
read.IGORitx <- function(itxname) {
  contents <- readLines(itxname)
  splitm <- function(m) split(m,row(m))
  getscaleparms <- function(x) {
    fields <- c("flags","dimension","start","inc","label","wave")
    patt <- "SetScale([/A-Z]*) ([a-z]) ([0-9.]+),([0-9.]+),\"(.*)\", (.*)"
    fmt <- "\\1,\\2,\\3,\\4,\\5,\\6"
    t(sapply(strsplit(sub(patt,fmt,x),","),`names<-`,fields))
  }
  getattrib <- function(attrib) {
    asdfr <- function(x) as.data.frame(do.call(rbind,x))
    output <- asdfr(Map(getscaleparms,strsplit(sub("^X ","",attrib),"; ")))
    output[] <- lapply(output,type.convert,as.is=TRUE)
    unclass(by(output[,c("flags","start","inc")],
               output[,c("wave","dimension")],as.list))
  }
  waves <- lapply(strsplit(grep("WAVES",contents,value=TRUE),"\t"),`[`,-1)
  begin <- grep("BEGIN",contents)
  end <- grep("END",contents)
##{{{
  ## values <- lapply(`names<-`(force(splitm(cbind(begin,end))),waves),
  ##                  function(i,x) sapply(tabsplit(x[(i[1]+1):(i[2]-1)]),rmempty),
  ##                  contents)
  ## values[] <- lapply(values,type.convert,as.is=TRUE)  
##}}}
  values <- local({
    idx <- splitm(cbind(begin, end))
    rec <- lapply(idx,function(i, contents)
                  sapply(Filter(function(.x) length(.x) > 0 && any(.x!=""),
                                strsplit(contents[(i[1] + 1):(i[2] - 1)],"\t")),
                         function(.x) .x[.x!=""]), ## use sapply for dimensionality
                  contents)
    rec[] <- lapply(rec, type.convert, as.is = TRUE)
    ## in case empty
    rec[] <- Map(function(w,i,r) if(diff(i)>1) r else
                 matrix(NA,nrow=length(w),ncol=1),
                 waves,idx,rec)
    ## save output with attributes
    out <- list()
    for( i in seq(along=waves) ) {
      if( length(waves[[i]]) == 1 ) {
        out[[waves[[i]]]] <- rec[[i]]
      } else if( length(waves[[i]]) > 1 ) {
        out <- c(out,`names<-`(force(split(rec[[i]],row(rec[[i]]))), waves[[i]]))
      }
    }
    out
  })
  attrib <- grep("^X",contents[end+1],value=TRUE)
  if( length(attrib) > 0 ) {
    mat <- getattrib(attrib)
    ex <- as.data.frame(`mode<-`(as.matrix(do.call(expand.grid,dimnames(mat))),
                                 "character"))
    for( elem in split(ex,1:nrow(ex)) ) {
      attr(values[[elem$wave]],elem$dimension) <-
        mat[[elem$wave,elem$dimension]]
      if( !"wave" %in% class(values[[elem$wave]]) )
        class(values[[elem$wave]]) <- c(class(values[[elem$wave]]),"wave")
    }
  }
  values
}
load.IGORitx <- function(itxname,verbose=TRUE,env=globalenv()) {
  obj <- read.IGORitx(itxname)
  for( elem in names(obj) ) {
    if( verbose ) cat("Loading ",elem,"...\n")
    assign(elem,obj[[elem]],env)
  }
  invisible(names(obj))
}
read.itx <- read.IGORitx
load.itx <- load.IGORitx

attrnoclass <- function(x) {
  a <- attributes(x)
  a[!grepl("class",names(a))]
}
