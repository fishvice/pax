#' Title
#'
#' @param st_length XXX
#' @param st_ototliths XXX
#' @param species XXX
#' @param std.towlength XXX
#' @param std.cv XXX
#' @param std.area XXX
#'
#' @export
calc_age_indices <- function(st_length, st_ototliths, species,
                             std.towlength = 4,
                             std.cv = 1,
                             std.area = 4 * 17/1852) {
  
  ldist <- make_ldist_by_station(st_length, species, lengthclass)
  alk <-   make_alk(st_ototliths, species, lengthclass)
  
  d <-
    ldist %>% 
    dplyr::left_join(alk %>% 
                       dplyr::select(-n), by = c("lclass", "ar", "region")) %>% 
    dplyr::mutate(n = n * p,
           nxlengd = nxlengd * p,
           nxlengd2 = nxlengd2 * p) %>% 
    dplyr::group_by(synis.id, aldur) %>% 
    dplyr::summarise(n = sum(n))
  
  base <-
    st_length %>% 
    dplyr::left_join(d, by=c("synis.id")) %>%
    dplyr::rename(id = synis.id) %>% 
    dplyr::arrange(id, aldur) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(towlength = ifelse(is.na(towlength), 4, towlength),
                  n  = ifelse(is.na(n), 0, n)  * std.towlength / towlength) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ar, strata, aldur) %>%
    dplyr::summarise(N  = n(),
                     n_m  = mean(n),
                     n_d  = ifelse(N == 1, n_m  * std.cv, stats::sd(n))) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(stratas %>% dplyr::select(strata, area = area), by = "strata") %>%
    dplyr::mutate(area  = area/1.852^2 / std.area,
                  n     = n_m  * area)
  
  aggr <-
    base %>%
    dplyr::group_by(ar, aldur) %>%
    # A la Höski:
    dplyr::summarise(n = sum(n),
                     n.cv = calc_cv(n_m,n_d,area,N))
  
  return(list(base = base, aggr = aggr))
  
}