#' @title Trim extreme towlengths
#'
#' @description Function that "trims" towlength to a certain minimum and/or maximum length
#'
#' @param x A vector representing towlength
#' @param std.towlength Standard towlength (default is 4)
#' @param min.towlength Minimum towlength. If missing (default) the value is set to
#' half of the std.towlength
#' @param max.towlength Maximum towlength. If missing (default) the value is set to
#' double the std.towlength
trim_towlength <- function(x, std.towlength = 4, min.towlength, max.towlength) {
  
  if(missing(min.towlength)) min.towlength <- std.towlength / 2
  if(missing(max.towlength)) max.towlength <- std.towlength * 2
  
  x <- ifelse(is.na(x),std.towlength, x)
  x <- ifelse(x > max.towlength, max.towlength, x)
  x <- ifelse(x < min.towlength, min.towlength, x)
  
  return(x)
}
