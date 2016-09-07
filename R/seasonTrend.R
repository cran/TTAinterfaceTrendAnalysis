seasonTrend <-
function(x, ...) {

  # validate args
  if (!is(x, "ts"))
    stop("x must be a 'ts'")

  # extend to full years
  first = start(x)[1]
  last = end(x)[1]
  fr <- frequency(x)
  x <- window(x, start = first, end = c(last, fr), extend = TRUE)

  # function for single vector
  st <- function(x) {
    x1 <- matrix(x, ncol = fr, byrow = TRUE)
    mannKen(x1)[, c(1:3, 6)]
  }

  # construct a data frame of all trends
  if (!is.matrix(x)) {
    ans <- data.frame(season = as.factor(1:fr), st(x), row.names = 1:fr)
  } else {
    nc <- ncol(x)
    colx <- colnames(x)
    series <- factor(rep(colx, each = fr), levels = colx, ordered = TRUE)
    season <- as.factor(rep(1:fr, times = nc))
    ans0 <- do.call(rbind, lapply(1:nc, function(i) st(x[, i])))
    ans <- data.frame(series, season, ans0, row.names = 1:nrow(ans0))
  }


}
