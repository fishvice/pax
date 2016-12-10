# The code used by KK - fall 2016
#attach("/net/hafkaldi/export/u2/reikn/Splus5/HAUSTRALLNewStrata/.RData")
#attach("/net/hafkaldi/export/u2/reikn/Splus5/HAUSTRALLNewStrata/Stratifiering/.RData")
#mostcommon <- function(x)
#{
#  x1 <- sort(table(x))
#  x1 <- x1[length(x1)]
#  return(as.numeric(names(x1)))
#}

# haustrall.all.st <- lesa.stodvar(veidarfaeri = c(77,78), oracle=TRUE) %>%
#   filter(synaflokkur %in% 35,
#          ar > 1995,
#          !ar %in% 2011, # Haustrall 2011 ógild mæling
#          tognumer < 80) %>%   # Grænlandstogum hent út.
#   mutate(index = reitur*100+tognumer,
#          mult = ifelse(veidarfaeri == 78, 1.25, 1))
# haustrall.all.st <- inside.strata(haustrall.all.st,haustralllist$nr)
# haustrall.all.st <- mutate(haustrall.all.st, ind = as.numeric(paste(index,veidarfaeri,sep="")),
#                            newstrata = replace(newstrata, ind == 2706178, 1), # ein stöð
#                            newstrata = replace(newstrata, ind == 2620178, 7)) # 15 stöðvar
# 
# x1 <- apply.shrink(haustrall.all.st$newstrata,haustrall.all.st$ind,
#                    mostcommon,names=c("ind","commonstrata"))
# haustrall.all.st <- left_join(haustrall.all.st,x1)
# i <- !is.na(haustrall.all.st$commonstrata) &
#   haustrall.all.st$newstrata != haustrall.all.st$commonstrata
# haustrall.all.st$newstrata[i] <- haustrall.all.st$commonstrata[i]
# ### Strata 21 og 28 sameinuð í strata 45 til að minnka svæðið.
# haustrall.all.st <- mutate(haustrall.all.st,
#                            newstrata = replace(newstrata, newstrata %in% c(21,28), 45))
# i <- is.na(haustrall.all.st$kl.kastad)
# if(any(i)) haustrall.all.st$kl.kastad[i] <- 12
# #rm(x1,i)
# haustrall.all.st <- inside.reg.bc(haustrall.all.st)

#' Select smh stations and assign strata to stations
#'
#' @export
#'
smh_strata_setup <- function() {
  
  d <-
    fjolst::lesa.stodvar(veidarfaeri = c(77,78), oracle=TRUE) %>%
    dplyr::filter(synaflokkur %in% 35,
                  ar > 1995,
                  !ar %in% 2011,       # Haustrall 2011 ógild mæling
                  tognumer < 80) %>%   # Grænlandstogum hent út.
    dplyr::mutate(index = reitur * 100 + tognumer,
                  mult = ifelse(veidarfaeri == 78, 1.25, 1)) %>% 
    husky::inside.strata(husky::haustralllist$nr, husky::NEWSTRATAS) %>% 
    dplyr::mutate(ind = as.numeric(paste0(index, veidarfaeri)),
                  newstrata = ifelse(ind == 2706178, 1, newstrata),      # ein stöð
                  newstrata = ifelse(ind == 2620178, 7, newstrata))      # 15 stöðvar
  
  # find the most common strata
  x2 <-
    d %>%
    dplyr::rename(top = newstrata) %>% 
    dplyr::group_by(ind, top) %>% # note the order here matters
    dplyr::summarise(n = n()) %>% 
    # arranged such that get same as kk when it comes to ties!
    dplyr::arrange(ind, desc(n), desc(top)) %>% 
    dplyr::slice(1)
  
  d %>% 
    dplyr::full_join(x2, by = "ind") %>% 
    dplyr::mutate(newstrata = top,
                  # Strata 21 og 28 sameinuð í strata 45 til að minnka svæðið
                  newstrata = ifelse(newstrata %in% c(21,28), 45, newstrata),
                  kl.kastad = ifelse(is.na(kl.kastad), 12, kl.kastad)) %>% 
    geo::inside.reg.bc() %>% 
    dplyr::tbl_df()
}

