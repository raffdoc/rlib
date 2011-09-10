collapse <- function(...)
  UseMethod("collapse")
collapse.array <- function (X, MARGIN, FUN = identity, ..., OPERATOR = `+`) {
    extract <- function(i) do.call(`[`, c(list(X), replace(rep(list(TRUE), 
        length(dim(X))), MARGIN, i),list(drop=FALSE)))
    Reduce(function(x,i) OPERATOR(x,FUN(extract(i),...)), seq(1, dim(X)[MARGIN]))
}
## collapse.array <- function(X,MARGIN,FUN=identity,...,OPERATOR=`+`) {
##   extract <- function(i)
##     do.call(`[`,c(list(X),replace(rep(list(TRUE),length(dim(X))),MARGIN,i)))
##   composed <- function(i,...)
##     FUN(extract(i),...)
##   Reduce(OPERATOR,lapply(seq(1,dim(X)[MARGIN]),composed,...))
## }
