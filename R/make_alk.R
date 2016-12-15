#' Make age length key
#'
#' @param stodvar XXX
#' @param SPECIES XXX
#' @param lengthclass XXX
#' @param vikmork XXX
#' @param kvarnir XXX
#'
#' @export
#'
make_alk <- function(stodvar, SPECIES, lengthclass, vikmork, kvarnir) {

  # create a grid??

  # may be of use
  medalle <- (lengthclass[-length(lengthclass)] + lengthclass[-1])/2
  lkey <-
    dplyr::data_frame(lengd = 1:200,
                      lclass = as.character(cut(lengd, breaks = lengthclass))) %>%
    dplyr::filter(!is.na(lclass))
  lkey2 <-
    lkey %>%
    dplyr::select(lclass) %>%
    dplyr::distinct() %>%
    dplyr::mutate(medalle = medalle)


  if(missing(kvarnir)) {
    kvarnir <-
      fjolst::lesa.kvarnir(stodvar$synis.id, SPECIES, col.names = c("kyn", "kynthroski", "slaegt", "oslaegt")) %>%
      dplyr::filter(!is.na(aldur),
                    aldur %in% 1:14) %>%  # should be user conrollable
      dplyr::left_join(stodvar, by = c("synis.id"))
  }

  if(!missing(vikmork)) {
    kvarnir <-
      kvarnir %>%
      dplyr::left_join(vikmork, by = c("aldur")) %>%
      dplyr::filter(lengd >= minl & lengd <= maxl) %>%
      dplyr::select(-minl, -maxl)
  }

  # ----------------------------------------------------------------------------
  # seeding smooth spline
  smooth_model <- function(df) {
    stats::smooth.spline(df$aldur ~ df$lengd, df = 2)
  }

  tmp <-
    kvarnir %>%
    dplyr::group_by(ar, region) %>%
    tidyr::nest() %>%
    dplyr::mutate(model = purrr::map(data, smooth_model)) %>%
    dplyr::select(ar, region, model)
  for(i in 1:nrow(tmp)) {
    x <-
      dplyr::data_frame(ar = tmp$ar[[i]],
                 region = tmp$region[[i]],
                 medalle = medalle,
                 aldur = round(stats:::predict.smooth.spline(tmp$model[[i]], medalle)$y),
                 lclass = as.character(cut(medalle, breaks = lengthclass)))
    if(i == 1) {
      res <- x
    } else {
      res <- bind_rows(res, x)
    }
  }

  # dplyrize below
  i <- res$aldur > max(1:14)
  if (any(i)) res$aldur[i] <- max(1:14)
  i <- res$aldur < min(1:14)
  if (any(i)) res$aldur[i] <- min(1:14)

  # end seeding the smooth spline
  # ----------------------------------------------------------------------------

  d <-
    kvarnir %>%
    dplyr::mutate(lclass = as.character(cut(lengd, breaks = lengthclass))) %>%
    dplyr::filter(!is.na(lclass)) %>%
    dplyr::group_by(ar, region, lclass, aldur) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by(ar, region, lclass)

  # here one should check for missing age in particular length class
  # ...
  # ...

  d <-
    d %>%
    dplyr::mutate(p = n/sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::tbl_df()

  # make a grid
  g <-
    dplyr::data_frame(lengd = 0:200) %>%
    dplyr::mutate(lclass = as.character(cut(lengd, breaks = lengthclass))) %>%
    dplyr::filter(!is.na(lclass)) %>%
    dplyr::select(lclass) %>%
    dplyr::distinct()
  g <-
    expand.grid(ar = unique(d$ar),
                region = unique(d$region),
                lclass = g$lclass,
                aldur = 1:14,
                stringsAsFactors = FALSE)
  d <-
    g %>%
    dplyr::left_join(d, by = c("ar", "region", "lclass", "aldur")) %>%
    dplyr::mutate(n = ifelse(is.na(n), 0, n),
                  p = ifelse(is.na(p), 0, p))

  return(list(d=d, res = res))

}
