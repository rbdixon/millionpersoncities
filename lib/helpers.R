# Return the index of the smallest non-zero item
# Like `order` but ignore zero.
minindex.nonzero <- function(x) {
  x[x == 0] <- NaN
  order(x, na.last=TRUE)[1]
}

printf <- function(s, ...) {
  print(sprintf(s, ...))
}