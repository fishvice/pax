#' Make length distribution by station
#'
#' @param stodvar XXX
#' @param SPECIES XXX
#' @param lengthclass XXX
#' @param lengdir XXX
#' @param numer XXX
#'
#' @export
#'
make_ldist_by_station <- function(stodvar, SPECIES, lengthclass, lengdir, numer) {
  
  if(missing(lengdir)) {
    lengdir <-
      fjolst::lesa.lengdir(st$synis.id, teg = SPECIES) %>%
      dplyr::group_by(synis.id, lengd) %>%
      dplyr::summarise(fjoldi = sum(fjoldi)) %>%
      dplyr::ungroup()
  }
  
  if(missing(numer)) {
    numer <-
      fjolst::lesa.numer(st$synis.id, teg = SPECIES) %>%
      dplyr::mutate(r = (fj.talid + fj.maelt)/fj.maelt) %>%
      dplyr::select(synis.id, r)
  }
  
  lengdir <-
    lengdir %>%
    dplyr::left_join(numer) %>%
    dplyr::mutate(fj.alls = r * fjoldi)
  
  
  d <-
    # Create a full grid
    #   may be more efficient to do this in the end
    expand.grid(synis.id = sort(stodvar$synis.id),
                lengd = 1:200) %>%
    dplyr::left_join(stodvar, by = "synis.id") %>%
    dplyr::select(synis.id, lengd) %>%
    dplyr::left_join(lengdir) %>%
    dplyr::mutate(fjoldi = ifelse(is.na(fjoldi), 0, fjoldi),
                  fj.alls = ifelse(is.na(fj.alls), 0, fj.alls)) %>%
    # here correct by toglengd ???
    dplyr::mutate(lclass = as.character(cut(lengd, breaks = lengthclass))) %>%
    dplyr::filter(!is.na(lclass)) %>%
    dplyr::group_by(synis.id, lclass) %>%
    dplyr::summarise(n = sum(fj.alls),
                     nxlengd = sum(fj.alls * lengd),
                     nxlengd2 = sum(fj.alls * lengd^2)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(stodvar)
  
  return(d)
  
}
