if (regexpr("mac",.Platform$pkgType)>0) options(device="quartz")
## options(device="X11")
options(##help_type="html",
        repos="http://cran.stat.ucla.edu",
        stringsAsFactors=FALSE,
        deparse.max.lines=10,
        warn=1)

Sys.setenv(PKG_CONFIG_PATH="/usr/local/ggobi/lib/pkgconfig:/Library/Framewo rks/GTK+.framework/Resources/lib/pkgconfig")

##http://ifellows.ucsd.edu/pmwiki/pmwiki.php?n=Main.BuildingDeducerFromTheSource
Sys.setenv(NOAWT=1)

##options(error=recover)

##assign("help",utils::help,globalenv())

if( all(regexpr("^Rprofile$",search()) < 0) )
  attach(NULL,name="Rprofile") # pos=2 by default

evalq({

  recsub <- function(x,replace) 
    Reduce(function(.x,i) sub(names(replace)[i],replace[i],.x),
           seq_along(replace),x)
  
  catpr <- function(x,fmt='"%s"')
    cat(sprintf("c(%s)",paste(sprintf(fmt,x),collapse=",")),"\n")
  
  capitalize <- function(x)
  `substr<-`(x,1,1,value=toupper(substr(x,1,1)))

  setpar <- function(p)
    par(p[!names(p) %in% c("cin","cra","csi","cxy","din")])
  
  setmargins <- function() graphics::par(mar=c(4,4,1.5,1.5),mgp=c(2.5,1,0))
  setmmargins <- function() graphics::par(mar=c(1,1,.5,.5),oma=c(3,3,1,1),mgp=c(2.2,.5,0))

  embedfonts <- function(x)
    embedFonts(x,format="pswrite",sub("\\.eps$","_emb.eps",x))

  compressPDF <- function(x) {
    system(paste("pdftk",x,"output",sub("\\.pdf","c.pdf",x),
                 "compress dont_ask"))
    unlink(x)
  }

  compresspdf <- function(x) {
    fillspace <- function(x) gsub("([ ]+)","\\\\\\1",x)
    tmpfile <- sprintf("%s~",x)
    system(paste("pdftk",fillspace(x),"output ",
                 fillspace(tmpfile),"compress dont_ask"))
    file.rename(tmpfile,x)
  }

  ## between <- function(x,rg,op=`<=`)
  ##   sapply(x,function(.x) op(prod(.x-rg),0))

  mat2list <- function(m,f=row) split(m,f(m))
  
  ## candidate names: fapply, feval, fcall
  ## funcall is taken from emacs lisp
  funcall <- function(f,...) f(...)

  alpha.col <- function(cols,alpha=0.5)
    rgb(t(col2rgb(cols))/255,alpha=alpha)

  char2col <- function(x,col=rainbow)
    (function(x) col(nlevels(x))[x])(factor(x))
  
  ## improved list of objects
  .ls.objects <- function (pos = 1, pattern, order.by,
                           decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
      out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
      out <- head(out, n)
    out
  }
  ## shorthand
  lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
  }

  ##{{{

  ## logaxis <- function(values=NULL,limits=NULL,fc=pretty,...,
  ##                     integers=TRUE,override=FALSE) {
  ##   ## ... to func [seq() or pretty()]
  ##   patt <- list(c("e\\+0+$",""),c("e\\+","%*%10^"),c("e","%*%10^"))
  ##   fs <- (if( integers ) c(fmin=floor,fmax=ceiling) else
  ##          c(fmin=identity,fmax=identity))
  ##   consider <- c(log10(c(limits,values)))
  ##   lims <- mapply(function(f,x) f(x), fs, range(na.omit(consider)))
  ##   at <- (if( identical(fc,seq) ) fc(lims[1],lims[2],...) else
  ##          fc(consider,...))
  ##   if( integers && !override && any(duplicated(round(at))) )
  ##     integers <- FALSE
  ##   labels <- (if( integers ) parse(text=sprintf("10^%d",at)) else
  ##              parse(text=Reduce(function(init,x) sub(x[1],x[2],init),
  ##                      patt,formatC(10^at,format="e"))))
  ##   list(at=at,labels=labels)
  ## }

  ## logMinorTcks <- function(x,y) {
  ##   if( !missing(y) ) x <- c(x,y)
  ##   outer(1:9,10^do.call(`:`,as.list(log10(x))))
  ## }

  ##}}}

  logaxis <- function(side,minor=FALSE,decades.only=TRUE,draw=TRUE,tck,
                      IGORlab=FALSE,...) {
    logtest <- par(c("xlog","ylog")[(side-1) %% 2+1])
    if (logtest) {
      vals <- axTicks(side)
      if( !minor ) {
        tens <- floor(log10(vals))
        at <- (if( decades.only ) 10^unique(tens) else
               with(list(units=round(vals/(10^tens))),
                    unique(units*10^tens)))
        labels <-
          (if( decades.only ) parse(text=sprintf("10^%d",log10(at))) else
           with(list(u=at,d=floor(log10(at))),
                parse(text=sprintf("%d %%*%% 10^%d",round(u/(10^d)),d))))
        if(missing(tck)) tck <- par("tck")
      } else {
        tens <- do.call(`:`,as.list(range(round(log10(vals))) + c(-1,1)))
        at <- outer(1:9,10^tens)
        labels <- FALSE
        if(missing(tck)) tck <- -0.01
      }
    } else {
      at <- axTicks(side)
      ## reform <- (abs(at) > 1e2 | abs(at) < 1e-2 &
      ##            (at > .Machine$double.eps | -at >= .Machine$double.neg.eps))
      reform <- (at > .Machine$double.eps | -at >= .Machine$double.neg.eps)
      labels <-
        (if( any(reform) ) 
         local({
           vals <- at[reform]
           tens <- floor(log10(vals))
           units <- round(vals/(10^tens))
           parse(text=replace(at,reform,sprintf("%d %%*%% 10^%d",units,tens)))
         }) else format(at))
      if(missing(tck)) tck <- par("tck")
    }
    if (minor && IGORlab) {
      out <- logaxis(side,minor=TRUE,draw=FALSE)
      at <- out$at[-1,]
      at <- at[at >= 10^par("usr")[1] & at <= 10^par("usr")[2]]
      atlab <- substring(formatC(at,digits=1,format="e"),1,1)
      mtext(atlab,side=1,at=at,line=0,cex=.6)
    } else {
      arglist <- c(list(side=side,at=at,labels=labels,tck=tck),list(...))
      if( draw ) do.call(axis,arglist) else arglist
    }
  }

  log10seq <- as.function(c(formals(seq.default),quote({
    invocation <- as.list(match.call())
    ft <- Filter(Negate(is.null),as.list(invocation)[c("from","to")])
    invocation[names(ft)] <- lapply(ft,log10)
    10^do.call(seq,invocation[-1])
  })))

  logseq <- function(base=exp(1)) 
    function (from = 1, to = 1, by = ((to - from)/(length.out - 1)), 
              length.out = NULL, along.with = NULL, ...) 
      {
        invocation <- as.list(match.call())
        ft <- Filter(Negate(is.null), as.list(invocation)[c("from", 
                                                            "to")])
        invocation[names(ft)] <- lapply(ft, function(x) log(x , base))
        base^do.call(seq, invocation[-1])
      }
  
  zip <- function(...) {
    dotArgs <- list(...)
    arglist <- (if( length(dotArgs)==1 &&
                   is.list(dotArgs[[1]]) )
                dotArgs[[1]] else dotArgs)
    do.call(function(...) Map(list,...),arglist)
  }
  enumerate <- function(x) zip(i=seq(along=x),x)


  env.exists <- function(env.name) any(grepl(env.name,search()))
  
  attach.env <- function(env.name, env=NULL)
    if( !env.exists(env.name) ) attach(env,name=env.name) # pos=2 by default
  
  populate.env <- function(env.name,path,...) {
    ## http://stackoverflow.com/questions/2634512/r-disentangling-scopes
    ## populates environment with functions in file or directory
    ## creates and attaches named environment to search() path 
    ##        if it doesn't already exist
    ## --- define environment ---
    env <- (if( env.exists(env.name) )
            as.environment(env.name) else
            new.env(hash=TRUE))
    ## --- source ---    
    if( file.info(path[1])$isdir ) {
      lapply(list.files(path,"\\.r$",full.names=TRUE,ignore.case=TRUE,...),
             sys.source,env)
    } else {
      lapply(path,sys.source,env)
    }
    attach.env(env.name,env)
    invisible()
  }

  strip.white <- function(x) gsub("^[ ]+|[ ]+$","",x)

  flatfill <- function(x,y=NULL,isempty=function(x) x=="")
    (if(length(x)==0) y else
     flatfill(tail(x,-1),c(y,if(isempty(head(x,1))) tail(y,1) else head(x,1)),
              isempty=isempty))

  factorize <- function(x,...) UseMethod("factorize")
  factorize.data.frame <- function(x,...) {
    ischar <- sapply(x,is.character)
    if( any(ischar) )
      x[ischar] <- lapply(x[ischar],factor)
    x
  }

  write.delim <- function(...,sep="\t",row.names=FALSE,quote=FALSE)
    write.table(...,sep=sep,row.names=row.names,quote=quote)

  selfname <- function(x,f=identity) `names<-`(force(x),f(x))

  ## inspired by roxygen
  newCompose <- function (...) {
    fs <- rev(list(...))
    function(...) Reduce(function(x, f) f(x), fs, list(...))
  }
  Compose <- function (...) {
    fs <- rev(list(...))
    function(...) Reduce(function(x, f) f(x), fs, ...)
  }
  ##{{{
  ## Curry <- function (FUN, ...,.right=FALSE) {
  ##   if( .right ) {
  ##     .orig = rev(list(...))
  ##     function(...) do.call(FUN, rev(c(.orig, list(...))))
  ##   } else {
  ##     .orig = rev(list(...))
  ##     function(...) do.call(FUN, c(.orig, list(...)))
  ##   }
  ## }
  ##}}}
  ##{{{
  ## Curry <- function (FUN, ...,.right=FALSE,.flip=FALSE) {
  ##   ## Test
  ##   ## tuple <- function(...) unlist(list(...))
  ##   ## Curry(tuple,1,2)(3,4)
  ##   ## Curry(tuple,1,2,.right=TRUE)(3,4)
  ##   ## Curry(tuple,1,2,.flip=TRUE)(3,4)
  ##   ## Curry(tuple,1,2,.right=TRUE,.flip=TRUE)(3,4)
  ##   .orig <- list(...)
  ##   fn <- if(.flip) rev else identity
  ##   function(...) 
  ##     do.call(FUN,if(.right) fn(c(list(...),rev(.orig))) else
  ##             fn(c(.orig,list(...))))
  ## }
  ##}}}
  Curry <- function (FUN, ..., .right=FALSE,.skipfirst=FALSE) {
    ## Test
    ## tuple <- function(...) unlist(list(...))
    ## Curry(tuple,1,2)(3,4)
    ## Curry(tuple,1,2,.right=TRUE)(3,4)
    .orig <- list(...)
    function(...) 
      do.call(FUN,if(.right) c(list(...),rev(.orig)) else
              if(.skipfirst) c(list(...),.orig) else
              c(.orig,list(...)))
  }

  logTicks <- function (lim, loc = c(1, 5)) { 
    ii <- floor(log10(range(lim))) + c(-1, 2) 
    main <- 10^(ii[1]:ii[2]) 
    r <- as.numeric(outer(loc, main, "*")) 
    r[lim[1] <= r & r <= lim[2]] 
  }
  
  scale.components.log10 <- function(side="bottom")
    function(lim, ...) {
      axfn <- eval(parse(text=sprintf("%s.components.default",
                           switch(side,bottom="xscale",left="yscale"))))
      ans <- axfn(lim = lim, ...)  
      tick.at <- logTicks(10^lim, loc = 1:9) 
      tick.at.major <- logTicks(10^lim, loc = 1) 
      major <- tick.at %in% tick.at.major 
      ans[[side]]$ticks$at <- log(tick.at, 10) 
      ans[[side]]$ticks$tck <- ifelse(major, 1.5, 0.75) 
      ans[[side]]$labels$at <- log(tick.at, 10) 
      ans[[side]]$labels$labels <- as.character(tick.at)
      ans[[side]]$labels$labels[major] <- parse(text=sprintf("10^%d",
                                                  log10(tick.at.major)))
      ans[[side]]$labels$labels[!major] <- "" 
      ans[[side]]$labels$check.overlap <- FALSE 
     ans 
    } 
  
  read.orgtable <- function(txt) {
    strip.bars <- function(x) gsub("^\\|[ ]*|[ ]*\\|$","",x)
    split.bars <- function(x) strsplit(strip.bars(x),"[ ]*\\|[ ]*")
    fill <- function(x) lapply(x,`length<-`,max(sapply(x,length)))
    clear <- function(x) Filter(function(x) length(x) > 0,x)    
    f <- (if( file.exists(txt) ) file(txt,open="rt") else textConnection(txt))
    lines <- with(list(x=readLines(f)), x[!grepl("^[#]+",x)])
    close(f)
    hline <- grep("^\\|\\-\\-.*", lines)
    if( length(hline) > 0 ) {
      header <- head(lines,hline-1)    
      body <- tail(lines,-hline)
      nms <- split.bars(header)[[1]]    
    } else {
      body <- lines
      nms <- NULL
    }
    dfr <- data.frame(do.call(rbind,fill(clear(split.bars(body)))))
    dfr[] <- lapply(dfr,type.convert,as.is=TRUE)
    if(!is.null(nms)) names(dfr) <- nms
    dfr
  }
  
  append.default <- base::append
  append <- `body<-`(args(append.default),value=quote(UseMethod("append")))
  append.data.frame <-
    `body<-`(args(append.default),value=
             quote(`row.names<-`(data.frame(append.default(x,values,after)),
                                 row.names(x))))

  last <- function(x) x[[length(x)]]

  asKey <- function(x) do.call(paste,c(unname(x),list(sep="_")))
  
  sampledfr <- function(dfr,f=.1)
    dfr[sort(sample(1:nrow(dfr),round(f*nrow(dfr)))),]

  sample <- `body<-`(args(base::sample),value=quote(UseMethod("sample")))
  sample.data.frame <- `body<-`(args(base::sample),
                                value=quote(x[sort(sample(1:nrow(x),size,
                                  replace,prob)),]))
  sample.default <- base::sample
  
  revargs <- function(f) UseMethod("revargs")
  revargs.function <- function(f)
    as.function(c(rev(head(as.list(args(f)),-1)),body(f)))  
  Flip <- revargs
  
  rev.factor <- function(x) factor(x,levels=rev(levels(x)))

  ren <- function(x,i,as) UseMethod("rename")
  ren.default <- function(x,i,as) {
    replacement <-
      (if( missing(i) ) as else if( is.character(i[1]) )
       replace(names(x),match(i,names(x)),as) else if( is.integer(i[1]) )
       replace(names(x),i,as))
    `names<-`(x,replacement)
  }
  ren.merged <- function(x,i=1,as="row.names")
    (if(as[1]=="row.names") `row.names<-`(x[,-i],x[,i]) else
     ren.default(x,i,as))
  key2rownames <- rename.merged <- ren.merged

  successivesub <- function(lis,x)
    Reduce(function(x,y) sub(y[1],y[2],x),lis,x)

  q <- function() base::quit("no")
  
  jitter.default <- base::jitter
  jitter <- `body<-`(args(jitter.default),value=quote(UseMethod("jitter")))
  jitter.factor <- `body<-`(args(jitter.default),
                            value=quote(jitter.default(as.numeric(x))))

  invisible(lapply(ls(),function(x,e,g)
                   if( class(eval(parse(text=x)))=="function" )
                   assign(x,`environment<-`(get(x,e),g),e),
                   environment(),globalenv()))

},as.environment("Rprofile"))


