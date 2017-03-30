#' allocate_strata
#' 
#' Function add column area, leidangrasynaflokkur, rtogl, index, oldstrata and
#' newstrata to data obtained from lesa.stodvar.
#' 
#' Tested for SMB 1985 to 2017 only
#'
#' @param years The survey years to use 
#'
#' @return
#'

allocate_strata <- function(years = 1985:2017) {
  
  # Einar: 2017-03-27
  # Stuff from /net/hafkaldi/export/u2/reikn/hoski/SurveyWork/SMB/Stations.r
  
  # Modifies such that no need to Rattach to directories to get access to objects
  #  since those already stored in library husky
  
  dat <- lesa.stodvar(
    ar = years,
    man = 2:4,
    veidarfaeri = 73,
    oracle = F
  )
  dat <-
    dat[dat$synaflokkur %in% 30,]
  
  # EH: leaves dataframe unchanged, potentially a redundant code
  sid <- c(34026, 34024)# Einkennilegar stöðvar
  dat <-
    dat[is.na(match(dat$synis.id, sid)), ]
  
  # Derived towlength
  dat$rtogl <-
    geo::arcdist(dat$kastad.n.breidd,
                 dat$kastad.v.lengd,
                 dat$hift.n.breidd,
                 dat$hift.v.lengd)
  dat$index <- dat$reitur * 100 + dat$tognumer
  
  # Remove stations ------------------------------------------------------------
  # Eyjarall 2009
  i <- dat$index %in% c(56835, 56836) #Eyrall 2009
  dat <- dat[!i, ]
  
  # Hanging comments from Höski
  # 61813 of sunnarlega eitt árið 56831 tekið einu sinni
  # 56931 einnig tekið 3.
  # 7 tog með NA vantar staðsetningu híft.
  #
  
  # ----------------------------------------------------------------------------
  # (old) strata allocation
  
  # A: Calculate the most common strata - FIRST GO -----------------------------
  dat <- husky::inside.strata(dat, husky::ralllist, husky::STRATAS)
  
  # Overwrite automatic strata allocation --------------------------------------
  #
  # Strötu "35 23 13 12  9 27  6" ekki með
  # EG outcommentd, stuff not used i <- c(35 , 23 , 13 , 12 , 9 , 27 , 6)
  # Strata 35 71911 fer í 36, 67015 í 34.  mostcommon leysir það mál
 
  i <- dat$newstrata %in% 23
  dat$newstrata[i] <- 88

  i <- dat$newstrata %in% 13
  dat$newstrata[i] <- 22

  i <- dat$newstrata %in% 12
  dat$newstrata[i] <- 8
  
  # Stöðvar teknar fá ár.
  i <- dat$index %in% 31921
  dat$newstrata[i] <- 8
  i <- dat$index %in% 27401
  dat$newstrata[i] <- 21
  
  i <- dat$index %in% c(71811, 66813)
  dat$newstrata[i] <- 40
  
  i <- dat$index %in% 71741
  dat$newstrata[i] <- 38
  i <- dat$index %in% 71912
  dat$newstrata[i] <- 36
  i <- dat$index %in% 71721
  dat$newstrata[i] <- 38
  
  i <- dat$index %in% 71735 # Ýmist 38 eða 40
  dat$newstrata[i] <- 38
  
  i <- dat$newstrata == 6
  dat$newstrata[i] <- 5

  # B: Calculate the most common strata - SECOND GO ----------------------------
  
  x <-
    apply.shrink(dat$newstrata,
                 dat$index,
                 husky::mostcommon,
                 names = c("index", "mostcommon"))
  dat <- fjolst:::join(dat, x, "index")
  
  # 
  ind <- c(31931, 31932, 32131, 36731, 37031, 37131, 37132,
           37231, 41431, 41531, 42231, 42232, 47431, 52331)
  i <-
    (dat$newstrata != dat$mostcommon) &
    (dat$index %in% ind | dat$tognumer < 20)
  dat$newstrata[i] <- dat$mostcommon[i]
  
  # tidy up, get rid of column "mostcommon" and rename "newstrata" as "oldstrata"
  i <- match("mostcommon", names(dat))
  dat <- dat[, -i]
  i <- match("newstrata", names(dat))
  names(dat)[i] <- "oldstrata"
  
  # END: (old) strata allocation
  # ----------------------------------------------------------------------------
  
  
  # ----------------------------------------------------------------------------
  # newstrata allocation
  
  # Automatic stata allocation -------------------------------------------------
  dat <-
    husky::inside.strata(dat, husky::smblist$nr, stratas = husky::NEWSTRATAS)
  
  # Overwrite automatic strata allocation --------------------------------------
  i <- dat$index == 27401
  dat$newstrata[i] <- 2
  i <- dat$index == 31712
  dat$newstrata[i] <- 5
  i <- dat$index == 31821
  dat$newstrata[i] <- 5
  i <-
    dat$index %in%  c(31921, 31922, 31936, 32003, 32021, 32102, 32203, 32211, 32212)
  dat$newstrata[i] <- 2
  i <- dat$index == 32511
  dat$newstrata[i] <- 45
  i <- dat$index == 66321
  dat$newstrata[i] <- 42
  i <- dat$index == 67521
  dat$newstrata[i] <- 17
  i <-
    dat$index %in% c(71621, 71811, 71911, 71912)
  dat$newstrata[i] <- 37
  
  # Calculate the most common strata -------------------------------------------
  x <-
    apply.shrink(dat$newstrata,
                 dat$index,
                 husky::mostcommon,
                 names = c("index", "mostcommon"))
  
  dat <- fjolst:::join(dat, x, "index")

  # Overwrite most common strata allocation ------------------------------------  
  i <- dat$index %in% 71741 & dat$ar == 2009
  dat$newstrata[i] <- 37
  i <- dat$index %in% 61141 & dat$ar == 2010
  dat$newstrata[i] <- 42
  i <- dat$index == 71721
  dat$newstrata[i] <- 37
  
  i <-
    (dat$newstrata != dat$mostcommon) &
    (dat$index %in% ind | dat$tognumer < 20)
  dat$newstrata[i] <- dat$mostcommon[i]
  
  # get rid of column "mostcommon"
  i <- match("mostcommon", names(dat))
  dat <- dat[, -i]
  
  # END: newstrata allocation
  # ----------------------------------------------------------------------------
  
  return(dat)
  
}


trim_towlength2 <-
  function(d,
           std.towlength = 4,
           min.towlength = 2,
           max.towlength = 8) {
    d %>%
      # seems like one can not refer to same variable twice within one single mutate
      mutate(toglengd = if_else(is.na(toglengd), 4, toglengd)) %>%
      mutate(toglengd = if_else(toglengd > max.towlength, max.towlength, toglengd)) %>%
      mutate(toglengd = if_else(toglengd < min.towlength, min.towlength, toglengd))
  }

standardise_towlength <- function(d, std.towlength = 4) {
  d %>% dplyr::mutate(N = N * std.towlength / toglengd)
  
}

gather_length_data <- function(Stations, Species, Length = 0) {
  # Here could create an switch, if Stations is connection do below
  #   else read from fjolst
  Lengths <-
    mar::lesa_lengdir(Stations$src) %>%
    dplyr::filter(tegund %in% Species,
                  lengd >= Length) %>%
    dplyr::group_by(synis_id, tegund, lengd) %>%
    dplyr::summarise(fjoldi = sum(fjoldi)) %>%
    dplyr::ungroup()
  
  Counts <-
    mar::lesa_numer(Stations$src) %>%
    dplyr::filter(tegund %in% Species) %>%
    dplyr::mutate(r =  ifelse(fj_maelt != 0, 1 + fj_talid / fj_maelt, 1)) %>%
    dplyr::select(synis_id, tegund, fj_maelt, fj_talid, r)
  
  Stations %>%
    dplyr::left_join(Lengths, by = "synis_id") %>%
    dplyr::left_join(Counts, by = c("synis_id", "tegund"))
  
}

scale_by_counted <- function(d) {
  d %>%
    dplyr::mutate(fjoldi = r * fjoldi / 1e3) %>%   # units of thousan
    dplyr::select(-r)                              # no use anymore
}

standardise_towlength <- function(d, std.towlength = 4) {
  d %>%
    dplyr::mutate(N = fjoldi * std.towlength / toglengd) %>%   # standardize to per 4 miles
    dplyr::select(-toglengd)
  
}


calculate_biomass <- function(d) {
  lwcoeff <- mar::tbl_mar(db, "ops$einarhj.lwcoeff")
  
  d %>%
    dplyr::left_join(lwcoeff, by = "tegund") %>%
    # use Newton's law if lwcoefficient for species not specified
    dplyr::mutate(a = ifelse(is.na(a), 0.01, a),
                  b = ifelse(is.na(b), 3.00, b)) %>%
    # Below statistic is by length class
    # If include lwcoeff then do a priori a left_join
    dplyr::mutate(B  = ifelse(is.na(N), 0, N) * a * lengd ^ b / 1e3) %>%
    dplyr::select(-a,-b)
  
}

calc_by_strata <- function(d) {
  # Stuff should not really be inside this function
  Stratas <-
    mar::tbl_mar(db, "ops$einarhj.stratas") %>%
    dplyr::select(strata, area = rall.area)
  
  d %>%
    dplyr::group_by(ar, strata) %>%
    dplyr::summarise(
      sN  = n(),
      # Number of stations within strata
      n_m  = mean(N),
      n_d  = ifelse(n() == 1, mean(N) * std.cv, sd(N)),
      b_m  = mean(B),
      b_d  = ifelse(n() == 1, mean(B) * std.cv, sd(B))
    ) %>%
    #b_d  = ifelse(N == 1, b_m  * std.cv, stats::sd(b)),
    dplyr::ungroup() %>%
    dplyr::left_join(Stratas, by = "strata") %>%
    # within oracle, have to mutate in stages
    dplyr::mutate(area  = area / 1.852 ^ 2 / std.area) %>%
    dplyr::mutate(n     = n_m  * area,
                  b     = b_m  * area)
  
}

aggregate_by_year <- function(d) {
  # below equvialent to aggr-calculation
  d %>%
    dplyr::group_by(ar) %>%
    # A la Höski:
    dplyr::summarise(
      n = sum(n),
      n.cv = calc_cv(n_m, n_d, area, sN),
      b = sum(b),
      b.cv = calc_cv(b_m, b_d, area, sN)
    ) %>%
    dplyr::ungroup()
  
}