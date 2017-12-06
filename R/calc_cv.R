#' Calculate overall cv from stratfied summary statistics
#' 
#' @param m Mean value within strata
#' @param s Standard deviation within strata
#' @param area The area (e.g. survey strata area)
#' @param n number of samples within strata
#'
#' @export
#'
calc_cv <- function(m, s, area, n) {
  Mean = sum(m * area)/sum(area)
  Sum = sum(m * area)
  tmpsum = sum(m[!is.na(s)] * area[!is.na(s)])
  Calc.sdev = sqrt(sum(s[!is.na(s)]^2 * area[!is.na(s)]^2/  n[!is.na(s)])   / sum(area[!is.na(s)])^2)
  Sdev = Calc.sdev * Sum/tmpsum
  cv = Sdev/Mean
  
  return(cv)
}
