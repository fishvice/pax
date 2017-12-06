#' @title Calculate length based survey indices
#'
#' @description Calculates abundance and biomass survey indices based on length
#' classes for a particular species in a given year.
#'
#' The function does in principle three things
#' \itemize{
#' \item Standardizes value (e.g. number of fish) by tow length.
#' \item Calculates stratified indices.
#' \item Aggregates the stratified indices to the total area.
#' }
#' 
#' @export
#'
#' @return Returns a \emph{list} with the following \emph{data.frame}s:
#' \itemize{
#' \item \code{base} that contains the statistics by each strata
#' \itemize{
#' \item \code{year}: Names/number of the strata
#' \item ... TODO
#' }
#' \item \code{aggr} The total survey index. The columns are:
#' \itemize{
#' \item \code{year}: Survey year
#' \item \code{length}: The length class
#' \item \code{n}: Abundance index for the given length class
#' \item \code{n.cv}: cv of the abundance index for a given length class
#' \item \code{b}: Biomass index for the given length class
#' \item \code{b.cv}: cv of the biomass index for a given length class
#' \item \code{cn}: Abundance index of fish smaller than or equal to a given
#' length class.
#' \item \code{cn.cv}: cv of the abundance index of fish smaller than or equal
#' to a given length class
#' \item \code{cb}: Biomass index of fish greater than or equal to a given
#' length class.
#' \item \code{cb.cv}: cv of the biomass index of fish greater than or equal
#' to a given length class
#' }
#' }
#'
#' @param st A dataframe with station information. Required columns are
#' id (unique station id), year, towlength and strata (the strata identifyer).
#' @param le A dataframe with length frequency measurements. Required columns are
#' id (station id), length (the length class) and n (the number of fish measured) where
#' the latter are the "raised" numbers.
#' @param lwcoeff A vector of length 2, containing parameter
#' a and b of the length weight relationship.
#' @param stratas, A dataframe containing columns strata (the strata identifyer)
#' and area (the strata area).
#' @param std.towlength Standard tow length in nautical miles.
#' @param std.area Standardized area swept in nautical square miles.
#' @param std.cv A multipler (default is 1) on the mean abundance/biomass if only one tow in
#' a strata. In such cases the cv is set equivalent to the "mean" value.

calc_indices <- function(st,
                         le,
                         lwcoeff = c(0.01, 3),
                         stratas,
                         std.towlength = 4,
                         std.area = 4 * 17/1852,
                         std.cv = 1) {
  
  ## dummy, for passing test without lot of notes
  id <- n <- towlength <- b <- year <- strata <- N <- n_m <- cn <-
    cn_m <- b_m <- cb <- cb_m <- area <- n_d <- b_d <- cn_d <- cb_d <- 
    synis.id <- fj.talid <- fj.maelt <- mult <- n.counted <- n.measured <-
    n.total <- lengd <- fjoldi <- kyn <- NULL
  
  # Because we are calculating the abundance less than and biomass greater than
  #  we first generate a data.frame based on all combination of synis.id and length
  #  classes
  base <-
    dplyr::as_data_frame(expand.grid(length = c(5:140), id = st$id)) %>%
    dplyr::left_join(st, by = "id") %>%
    dplyr::left_join(le, by=c("id","length")) %>%
    dplyr::arrange(id, length) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(towlength = ifelse(is.na(towlength), 4, towlength),
                  n  = ifelse(is.na(n),0,n)  * std.towlength / towlength, # standardized to per 4 miles
                  cn = cumsum(n),
                  b  = n * lwcoeff[1] * length^lwcoeff[2]/1e3,
                  cb = sum(b) - cumsum(b) + b,
                  bc = cumsum(b)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year, strata, length) %>%
    dplyr::summarise(N  = n(),
                     n_m  = mean(n),
                     n_d  = ifelse(N == 1, n_m  * std.cv, stats::sd(n)),
                     cn_m = mean(cn),
                     cn_d = ifelse(N == 1, cn_m * std.cv, stats::sd(cn)),
                     b_m  = mean(b),
                     b_d  = ifelse(N == 1, b_m  * std.cv, stats::sd(b)),
                     cb_m = mean(cb),
                     cb_d = ifelse(N == 1, cb_m * std.cv, stats::sd(cb)),
                     bc_m = mean(bc),
                     bc_d = ifelse(N == 1, bc_m * std.cv, stats::sd(bc))
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(stratas %>% dplyr::select(strata, area = area), by = "strata") %>%
    dplyr::mutate(area  = area/1.852^2 / std.area,
                  n     = n_m  * area,
                  cn    = cn_m * area,
                  b     = b_m  * area,
                  cb    = cb_m * area,
                  bc    = bc_m * area)
  
  aggr <-
    base %>%
    dplyr::group_by(year, length) %>%
    # A la Höski:
    dplyr::summarise(n = sum(n),
                     n.cv = calc_cv(n_m,n_d,area,N),
                     b = sum(b),
                     b.cv = calc_cv(b_m,b_d,area,N),
                     cn = sum(cn),
                     cn.cv = calc_cv(cn_m, cn_d, area, N),
                     cb = sum(cb),
                     cb.cv = calc_cv(cb_m, cb_d, area, N),
                     bc = sum(bc),
                     bc.cv = calc_cv(bc_m, bc_d, area, N)) %>% 
    ungroup()
  
  return(list(base = base, aggr = aggr))
  
}
