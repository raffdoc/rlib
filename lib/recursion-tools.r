## tools for recursing through function s-expressions in R

recurselist <- function(x,FUN) {
  rapply(x,FUN,how="list")
}

explorenode <- function(x) {
  if(missing(x))
    substitute(x) else if(length(expr <- substitute(x))==1)
      expr else recurselist(as.list(expr),explorenode)
}

## example
## recurselist(as.list(body(graphics::axis)),explorenode)

