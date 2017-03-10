# The function make_ldist_by_station does very similar things

#' @title Catch per station for each length class
#'
#' @description Calculates the catch per station.
#'
#' Similar as \code{afli.per.stod} in the fjolst-package except returns
#' abundance and biomass per length class, as well as abundance less than
#' and biomass greater than a given length class.
#' 
#' @export
#'
#' @param Stations A dataframe with station information. Required columns are
#' id (unique station id), year, towlength and strata (the strata identifyer).
#' @param Lengths A dataframe with length frequency measurements. Required columns are
#' id (station id), length (the length class) and n (the number of fish measured) where
#' the latter are the "raised" numbers.
#' @param lwcoeff A vector of length 2, containing parameter
#' a and b of the length weight relationship.
#' @param std Character specifying standardization. Valdi character are "none",
#' "towlength" or "areaswept".
#' @param std.towlength Standard towlength in nautical miles. Default is 4 (not
#' used if std = "none").
#' @param std.towwidth Standard width of trawl in meters. Default is 17 (only
#' used if std = "areaswept").
#'
#' @return A dataframe containing the follow columns ..

catch_by_station <- function(Stations,
                             Lengths,
                             lwcoeff = c(0.01, 3),
                             std = c("none", "towlength", "areaswept"),
                             std.towlength = 4,
                             std.towwidth = 17) {
  
  ## dummy, for passing test without lot of noise
  id <- n <- towlength <- b <- NULL
  
  # ----------------------------------------------------------------------------
  # Standardize to what:
  
  converter <- 1852   # meters per nautical mile
  
  if(std == "none") {
    Stations <- Stations %>% dplyr::mutate(towlength = 1)  # makes nonsense but works further down
    std.towlength <- 1
    std.towwidth <- 1
    converter <- 1
  }
  if(std == "towlength") {
    std.towwidth <- 1
    converter <- 1
  }
  
  # if std == "none"      multiplier will be 1
  # if std == "towlength" multiplier will be std.towlength (e.g. per 4 nautical miles)
  # if std == "areaswept" multiplier will be as below with converter being 1852 meters
  #                         per square nautical miles
  multiplier <- std.towlength * std.towwidth / converter
  
  d <-
    #     NOTE: question were to specify length bins, here hardwired 5:140
    dplyr::as_data_frame(expand.grid(length = c(5:140), id = Stations$id)) %>%
    dplyr::left_join(Stations, by = "id")
  
  # Here should allow for length data.frame being missing
  #  But then also need numer because default is that stuff is raised
  #  Should may be have that as an option
  if(missing(Lengths)) {
    
  }
  
  d <-
    d %>%
    dplyr::left_join(Lengths, by=c("id","length")) %>%
    dplyr::arrange(id, length) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n  = ifelse(is.na(n),0,n)  / towlength * multiplier,
                  cn = cumsum(n),
                  b  = n * lwcoeff[1] * length^lwcoeff[2]/1e3,
                  cb = sum(b) - cumsum(b) + b) %>%
    dplyr::ungroup()
  
  return(d)
  
}
