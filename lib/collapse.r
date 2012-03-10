collapse <- function(...)
  UseMethod("collapse")
collapse.array <- function (X, MARGIN, FUN = identity, ..., fun.aggregate = `+`) {
    extract <- function(i) do.call(`[`, c(list(X), replace(rep(list(TRUE), 
        length(dim(X))), MARGIN, i),list(drop=FALSE)))
    Reduce(function(x,i) fun.aggregate(x,FUN(extract(i),...)),
           seq(1, dim(X)[MARGIN]))
}
