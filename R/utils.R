#' Convert correlation matrix to covariance matrix (lavaan compat)
#'
#' Uses lavaan::lav_cor2cov() when available; otherwise falls back to lavaan::cor2cov().
#' @keywords internal
cor2cov_lavaan <- function(R, sds, names = NULL) {
  stopifnot(is.matrix(R), nrow(R) == ncol(R))
  if (length(sds) != nrow(R)) stop("length(sds) must match nrow(R).")
  
  if ("lav_cor2cov" %in% getNamespaceExports("lavaan")) {
    lavaan::lav_cor2cov(R = R, sds = sds, names = names)
  } else {
    lavaan::cor2cov(R = R, sds = sds, names = names)
  }
}