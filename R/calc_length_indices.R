#' @title Calculate length based survey indices
#'
#' @description Calculates abundance and biomass survey indices based on length
#' classes for a particular species in a given year.
#' 
#' The minimum arguments needed are a Station table, a Strata table, Species code and
#' length-weight coefficient, with the rest of the data read in from fjolst.
#'
#' The function does in principle three things
#' \itemize{
#' \item Standardizes value (e.g. number of fish) by tow length.
#' \item Calculates stratified indices.
#' \item Aggregates the stratified indices to the total area.
#' }
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
#' @export
#' 
#' @param Station A dataframe with station information. Required columns are
#' id (unique station id), year, towlength and strata (the strata identifyer).
#' @param Stratas A dataframe containing columns strata (the strata identifyer)
#' and area (the strata area).
#' @param SPECIES Species code
#' @param lwcoeff A vector of length 2, containing parameter
#' a and b of the length weight relationship.
#' @param Sex Specify sex (1 or 2) to limit calulation. Useful e.g. if one is interested
#' in calculatin indices for "Raudmagi" or "Grasleppa". If missing (default) one
#' gets the ususal non-sexed indices.
#' @param Length A dataframe with length frequency measurements. If missing (default)
#' the data are read in using fjolst::lesa.lengdir based on the id (synis.id) in
#' Station table. This is the recomended method, but if a length dataframe is passed to the
#' function the required columns are id (station id), length (the length class) and n 
#' (the number of fish measured), the latter not being raised.
#' @param Subsampling A dataframe containing a raising factor (r) for each sample
#' id (synis.id) for the SPECIES. If missing (default) the data are read using the
#' fjolst::lesa.numer based on the id (synis.id) in the Station table. This is the
#' recomended method.
#' @param std.towlength Standard tow length in nautical miles (default 4)
#' @param std.towwidth Standardized tow width in meters (defalt 17)
#' @param std.cv A multipler (default is 1) on the mean abundance/biomass if only one tow in
#' a strata. In such cases the cv is set equivalent to the "mean" value.

calc_length_indices <- function(Station,
                                Stratas,
                                SPECIES,
                                lwcoeff,
                                Sex,
                                Length,
                                Subsampling,
                                std.towlength = 4,
                                std.towwidth = 17,
                                std.cv = 1) {
  
  ## dummy, for passing test without lot of noise
  id <- n <- towlength <- b <- year <- strata <- N <- n_m <- cn <-
    cn_m <- b_m <- cb <- cb_m <- area <- n_d <- b_d <- cn_d <- cb_d <- 
    synis.id <- fj.talid <- fj.maelt <- mult <- n.counted <- n.measured <-
    n.total <- lengd <- fjoldi <- kyn <- r <-  species <- sex <-  NULL
  
  if(missing(Subsampling)) {
    Subsampling <-
      fjolst::lesa.numer(Station$id, SPECIES, oracle = FALSE) %>% 
      dplyr::mutate(species = SPECIES) %>% 
      dplyr::select(id = synis.id, species,
                    n.counted = fj.talid,
                    n.measured = fj.maelt) %>% 
      # Scaling the two trawls - specific for the fall survey
      dplyr::left_join(Station %>% dplyr::select(id, mult), by = "id") %>% 
      dplyr::mutate(n.total = (n.counted + n.measured) / mult,
                    # Calculate the raising factor
                    r = n.total/n.measured,
                    r = ifelse(r == Inf, 1, r)) %>% 
      dplyr::select(id, r)
  }
  
  if(missing(Length)) {
    Length <- 
      fjolst::lesa.lengdir(Station$id, SPECIES, col.names="kyn", oracle = FALSE) %>% 
      dplyr::mutate(species = SPECIES) %>% 
      dplyr::select(id = synis.id, species, length = lengd,
                    n = fjoldi, sex = kyn)
    # Here one could do a filter on the sex, e.g. if one were interested in
    # calculating index for rauðmagi or grásleppa.
    # I.e.:
    #   full stop above, then
    if(!missing(Sex)) {
      Length <- 
        Length %>% 
        dplyr::filter(sex %in% Sex)
    }
    # and then proceed below
    # a double precaution, in case length bins by sex
    Length <-
      Length %>% 
      dplyr::group_by(id, length) %>%
      dplyr::summarize(n = sum(n)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(Subsampling, by="id") %>%
      dplyr::mutate(n = n * r / 1e3) %>%     # units of thousands
      dplyr::select(-r)
  }
  
  # calculate
  calc_indices(st = Station, lwcoeff = husky::LWCOEFF[[as.character(SPECIES)]], le = Length, stratas = Stratas)
}
