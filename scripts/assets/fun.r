library(lubridate)

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

#' Compute fmeasure
f_measure <- function(vp, fp, fn) {
  return((2 * vp) / (2 * vp + fp + fn))
}

#' Convert datetime TS to cumulative TS in seconds with t0 = 0
to_num <- function(datetime) {
  time <- lubridate::second(datetime)

  diff <- list()
  for (i in seq_along(time)) {
    if (i == 1) {
      diff[[i]] <- 0
      next
    }

    d <- time[i] - time[i - 1]

    # Handle 59s against 0s
    if (d < 0) {
      d <- 60 - time[i - 1]
    }

    diff[[i]] <- d
  }

  time <- cumsum(unlist(diff))

  return(time - time[1])
}
