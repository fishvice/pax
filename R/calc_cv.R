calc_cv <- function(x, xd, area, N) {
  Mean = sum(x * area)/sum(area)
  Sum = sum(x * area)
  tmpsum = sum(x[!is.na(xd)] * area[!is.na(xd)])
  Calc.sdev = sqrt(sum(xd[!is.na(xd)]^2 * area[!is.na(xd)]^2/  N[!is.na(xd)])   / sum(area[!is.na(xd)])^2)
  Sdev = Calc.sdev * Sum/tmpsum
  cv = Sdev/Mean
  
  return(cv)
}
