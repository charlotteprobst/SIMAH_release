#' function to calculate implausibility for education transitions calibration
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model implausibility
#' @export
#' @examples
#' calculate_implausibility_education
calculate_implausibility_education <- function(data, targets){
  data <- data %>%
    group_by(year, samplenum, seed, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
    summarise(n=sum(n)) %>%
    ungroup() %>%
    group_by(year, samplenum, seed, microsim.init.sex, microsim.init.race, agecat, microsim.init.education) %>%
    mutate(propsimulation = n/sum(n)) %>%
    mutate_at(vars(year), as.character) %>%
    mutate_at(vars(year,samplenum), as.numeric) %>%
    mutate_at(vars(microsim.init.sex,microsim.init.race, agecat, microsim.init.education, AlcCAT), as.character)

  variance <- data %>%
    group_by(year, samplenum, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
    summarise(variance = var(propsimulation)) %>%
    ungroup() %>%
    group_by(year, microsim.init.sex, microsim.init.race, agecat, microsim.init.education, AlcCAT) %>%
    summarise(v_s = mean(variance, na.rm=T))

  data <- left_join(data,targets)

  data <- left_join(data, variance)

  compare <- data %>%
    dplyr::select(-c(n,se,v_s,type)) %>%
    pivot_longer(propsimulation:proptarget) %>%
    group_by(year,samplenum,microsim.init.sex,microsim.init.race, agecat, microsim.init.education,
             AlcCAT,name) %>%
    summarise(value=mean(value, na.rm=T))
  #
  ggplot(subset(targets, microsim.init.sex=="f" & microsim.init.education=="LEHS"),
         aes(x=year, y=proptarget, colour=AlcCAT)) +
    geom_line(linewidth=1) +
    facet_grid(cols=vars(agecat), rows=vars(microsim.init.race), scales="free") + ylim(0,NA)
    # facet_wrap(~microsim.init.education+microsim.init.race+microsim.init.sex) +
    geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se, fill=AlcCAT),colour=NA, alpha=0.4)

  #
  # implausibility <- data %>%
  #   # filter(AGECAT=="18-24") %>%
  #   filter(year<=2019) %>%
  #   group_by(year, samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, agecat,
  #            AlcCAT) %>%
  #   summarise(propsimulation = mean(propsimulation),
  #             proptarget = mean(proptarget),
  #             se = mean(se),
  #             v_s = mean(v_s),
  #             # v_o = mean(variance),
  #             # todo - check implausibility equation in Andrianakis paper
  #             # should be SE^2?
  #             implausibility = abs(propsimulation-proptarget)/sqrt(v_s+se^2)) %>%
  #   group_by(samplenum) %>%
  #   summarise(implausibility=max(implausibility, na.rm=T)) %>%
  #   ungroup() %>%
  #   mutate(percentile=ntile(implausibility,100))
  return(implausibility)
}
