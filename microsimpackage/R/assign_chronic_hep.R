#' Assign new chronic cases of hepatitis in each year of the simulation 
#' based on acute hepatitis plus clearance rate 
#' @param
#' @keywords assign chronic hepatitis
#' @export
#' @examples
#' assign chronic hepatitis - throughout the simulation
assign_chronic_hep <- function(microsim){
  hep <- microsim %>% ungroup() %>% dplyr::filter(HepBstatus==1 | HepCstatus==1)
  if(dim(hep)[1]==0){
    microsim$chronicHep <- 0
  }else{
  # if(dim(hep)[1]!=0){
    hep$excessuse <- ifelse(hep$microsim.init.alc.gpd>40 & hep$microsim.init.sex=="f",1,
                            ifelse(hep$microsim.init.alc.gpd>60 & hep$microsim.init.sex=="m",1,
                                   0))
    hep$clearancerate <- ifelse(hep$excessuse==0, 0.361,
                                0.242)
    hep$chronicHep <- 0
    #  Hep B 
    # if(dim(subset(hep, HepBstatus==1))[1]!=0){
      hepBcleared <- hep %>% group_by(excessuse) %>% dplyr::filter(HepBstatus==1) %>% 
        add_tally(name="n") %>% 
        mutate(toclear = round(n * clearancerate)) %>% 
        do(dplyr::sample_n(.,size=unique(toclear), replace=FALSE))
      IDsclearedB <- hepBcleared$microsim.init.id
      hep$clearedB <- hep$microsim.init.id %in% IDsclearedB
      hep$chronicB_new <- ifelse(hep$HepBstatus==1 & hep$clearedB==FALSE, 1,0)
      hep$chronicHep <- ifelse(hep$chronicB_new==1, 1, hep$chronicHep)
      #  Hep C
    # }else if(dim(subset(hep, HepCstatus==1))[1]!=0){
      hepCcleared <- hep %>% group_by(excessuse) %>% dplyr::filter(HepCstatus==1) %>% 
        add_tally(name="n") %>% 
        mutate(toclear = round(n * clearancerate)) %>% 
        do(dplyr::sample_n(.,size=unique(toclear), replace=FALSE))
      IDsclearedC <- hepCcleared$microsim.init.id
      hep$clearedC <- hep$microsim.init.id %in% IDsclearedC
      hep$chronicC_new <- ifelse(hep$HepCstatus==1 & hep$clearedC==FALSE, 1,0)
      hep$chronicHep <- ifelse(hep$chronicC_new==1, 1, hep$chronicHep)
    # }
    # merge back with microsim file read in 
    hep <- hep %>% dplyr::select(microsim.init.id, chronicHep, chronicB_new, chronicC_new) %>% dplyr::filter(chronicHep==1)
    microsim <- left_join(microsim, hep, by=c("microsim.init.id")) %>% 
      mutate(chronicB_new = ifelse(is.na(chronicB_new), 0, chronicB_new),
             chronicC_new = ifelse(is.na(chronicC_new), 0, chronicC_new))
    microsim$chronicB <- ifelse(microsim$chronicB_new==1, 1,microsim$chronicB)
    microsim$chronicC <- ifelse(microsim$chronicC_new==1, 1,microsim$chronicC)
    microsim <- microsim %>% dplyr::select(-c(HepBstatus, HepCstatus, chronicHep, chronicB_new,
                                              chronicC_new))
   }
  return(microsim)
}
