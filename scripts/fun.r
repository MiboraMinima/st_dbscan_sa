median_filter <- function(x, y, w = 7, endr = "keep") {
  xmed <- runmed(x, w, endrule = endr)
  ymed <- runmed(y, w, endrule = endr)
  distmed <- unlist(
    lapply(
      c(seq_along(x)),
      function(i) {
        round(dist(rbind(cbind(x[i], y[i]), cbind(xmed[i], ymed[i]))), 1)
      }
    )
  )
  return(distmed)
}

f_measure <- function(vp, fp, fn) {
  return((2 * vp) / (2 * vp + fp + fn))
}
