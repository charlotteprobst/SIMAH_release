#' Summarise mean alcohol consumption by policy setting 
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples

summarise_alcohol_policy <- function(Output, SelectedState, version = standard){
  
  library(ggpubr)
  
  # set ggplot layout
  options(digits = 4)
  
  #c4 <- c("#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  c4 <- c("#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  c5 <- c("#BFBFBF", "#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  c6 <- c("#FFD679", "#BFBFBF", "#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  
  ggtheme <- theme_bw() + theme(legend.position="right",
                                strip.background = element_rect(fill="white"),
                                panel.spacing = unit(1,"lines"),
                                text = element_text(size=20))
  
  # generate output for continuous alcohol use
  
  if(output=="alcoholcont") {
    
  # targetdata <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2022_final.RDS") %>%
  #   filter(State==SelectedState) %>% filter(YEAR>=2000 & YEAR<2020) %>%
  #   filter(drinkingstatus==1) %>% 
  #   mutate(age_cat = cut(age_var,
  #                        breaks=c(0,24,64,100),
  #                        labels=c("18-24","25-64","65+")),
  #          sex = ifelse(sex_recode == "Male", "Men", "Women")) %>%
  #   rename(year = YEAR,
  #          education = education_summary,
  #          race = race_eth) %>%
  #   group_by(year, sex, education) %>%
  #   summarise(meangpd = mean(gramsperday_upshifted)) %>%
  #   mutate(scenario = "brfss")
  
    Output <- Output %>%
      filter(setting == version | setting == "counterfactual") %>% 
      mutate(year = as.numeric(as.character(year)),
             sex = ifelse(sex=="m","Men","Women"),
             education = factor(education, 
                                levels = c("LEHS", "SomeC", "College"),
                                labels = c("High school or less", "Some college", "College +")),
             scenario = factor(case_when(
               setting == "counterfactual" ~ "counterfactual",
               setting == "standard" ~ paste0("model", policymodel),
               TRUE ~ NA), levels = c("counterfactual", "model1", 
                                      "model2", "model3", "model4"))) 
    
    # 1) MEAN ALCOHOL CONSUMPTION LEVELS
    
    # mean across model runs (seeds)
    output1.mean <- Output %>%
      group_by(scenario, year, sex, education) %>% 
      summarise(meangpd = mean(as.numeric(meansimulation)))
    
    # min/max across model runs (seeds)
    var <- Output %>% filter(year == year_policy) %>% 
      group_by(scenario, year, sex, education) %>% 
      mutate(min = min(as.numeric(meansimulation)),
             max = max(as.numeric(meansimulation)),
             var = ifelse(meansimulation == min, "max",
                          ifelse(meansimulation == max, "min", NA))) %>% ungroup() %>%
      filter(meansimulation == min | meansimulation == max) %>%
      dplyr::select(c(seed, scenario, sex, education, var)) 
    
    output1 <- merge(Output, var, all.y = T) %>% 
      rename("meangpd" = "meansimulation") %>% 
      dplyr::select(c(scenario, year, sex, education, meangpd, var)) %>%
      pivot_wider(names_from = "var", values_from = "meangpd") %>%
      left_join(output1.mean, .)
    
    plot1 <- ggplot(data = output1, aes(x = year, colour = scenario)) + 
      geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = meangpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of alcohol per day") +
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + scale_color_manual(values = c5, name="") + 
      ggtitle("Simulated reduction in alcohol use (total population)")

    # 2) CHANGE IN ALCOHOL CONSUMPTION LEVELS
    
    counterfactual <- output1 %>% filter(scenario == "counterfactual") %>% ungroup() %>%
      rename("meangpdref" = "meangpd", "maxref" = "max", "minref" = "min") %>% 
      dplyr::select(-scenario)
    
    output2 <- output1 %>% filter(scenario != "counterfactual") %>% 
      left_join(., counterfactual) %>% 
      mutate(diffgpd = meangpd - meangpdref,
             diffmin = min - minref,
             diffmax = max - maxref,
             percgpd = (meangpd - meangpdref) / meangpdref,
             percmin = (min - minref) / minref,
             percmax = (max - maxref) / maxref) %>% 
      dplyr::select(c(scenario, year, sex, education,
                      diffgpd, diffmin, diffmax,
                      percgpd, percmin, percmax)) 
    
    plot2 <- ggplot(data = output2, aes(x = year, colour = scenario)) + 
      geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffgpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ylab("Change in mean grams per day (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use (total population)")
    
    plot3 <- ggplot(data = output2, aes(x = year, colour = scenario)) + 
      geom_line(aes(y = percmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percgpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      scale_y_continuous(labels = scales::percent) + 
      ylab("Change in mean grams per day in % (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use (total population)")

    list <- list(output1, output2, plot1, plot2, plot3)
    
  }
  
  # generate output for categorial alcohol use

  if(output=="alcoholcat") {
    
    Output <- Output %>%
      filter(setting == version | setting == "counterfactual") %>% 
      mutate(year = as.numeric(as.character(year)),
             sex = ifelse(sex=="m","Men","Women"),
             education = factor(education, 
                                levels = c("LEHS", "SomeC", "College"),
                                labels = c("High school or less", "Some college", "College +")),
             alc_cat = factor(alc_cat, 
                              levels = c("High risk", "Medium risk", "Low risk", "Non-drinker")), 
             scenario = factor(case_when(
               setting == "counterfactual" ~ "counterfactual",
               setting == "standard" ~ paste0("model", model),
               TRUE ~ NA), levels = c("counterfactual", "model1", 
                                      "model2", "model3", "model4"))) 
    
      # 1) MEAN PROPORTION BY ALCOHOL CATEGORY
      
      # mean across model runs (seeds)
      output1.mean <- Output %>%
        group_by(scenario, year, sex, education, alc_cat) %>% 
        summarise(meanprop = mean(as.numeric(prop)))
      
      # min/max across model runs (seeds)
      var <- Output %>% filter(year == year_policy) %>% 
        group_by(scenario, year, sex, education, alc_cat) %>% 
        mutate(min = min(as.numeric(prop)),
               max = max(as.numeric(prop)),
               var = ifelse(prop == min, "max",
                            ifelse(prop == max, "min", NA))) %>% ungroup() %>%
        filter(prop == min | prop == max) %>%
        dplyr::select(c(seed, scenario, sex, education, alc_cat, var)) 
      
      output1 <- merge(Output, var, all.y = T) %>% 
        rename("meanprop" = "prop") %>% 
        dplyr::select(c(scenario, year, sex, education, alc_cat, meanprop, var)) %>%
        pivot_wider(names_from = "var", values_from = "meanprop") %>%
        left_join(output1.mean, .)
      
      # men
      plot1m <- ggplot(data = output1[output1$sex == "Men",], 
                      aes(x = year, fill = alc_cat)) + 
        geom_bar(aes(y = meanprop), stat = "identity") +
        geom_errorbar(aes(ymin = min, ymax = max)) + 
        facet_grid(cols = vars(education), rows = vars(scenario), scales = "free") + 
        scale_x_continuous(breaks = seq(2000, 2015, 5)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.00001)) +
        ggtheme + xlab("") + ylab("Population in alcohol category (%)") +
        scale_fill_manual(values = c4, name = "", guide = guide_legend(reverse = TRUE)) + 
        ggtitle("Simulated reduction in alcohol use (total population)", "Men")
    
      # women
      plot1w <- ggplot(data = output1[output1$sex == "Women",], 
                       aes(x = year, fill = alc_cat)) + 
        geom_bar(aes(y = meanprop), stat = "identity") +
        geom_errorbar(aes(ymin = min, ymax = max)) + 
        facet_grid(cols = vars(education), rows = vars(scenario), scales = "free") + 
        scale_x_continuous(breaks = seq(2000, 2015, 5)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.00001)) +
        ggtheme + xlab("") + ylab("Population in alcohol category (%)") +
        scale_fill_manual(values = c4, name = "", guide = guide_legend(reverse = TRUE)) + 
        ggtitle("", "Women")
      
      plot1 <- ggarrange(plot1m, plot1w, ncol = 1, common.legend = T, legend = "bottom")
    
    list <- list(output1, plot1)
    
  }

  # generate output for continuous alcohol use by constant alcohol category

  
  if(output=="alcoholcontcat") {
    
    Output <- Output %>%
      filter(!is.na(!!alccatref) & !!alccatref != "Non-drinker") %>% 
      filter(setting == version | setting == "counterfactual") %>% 
      mutate(year = as.numeric(as.character(year)),
             sex = ifelse(sex=="m","Men","Women"),
             education = factor(education, 
                                levels = c("LEHS", "SomeC", "College"),
                                labels = c("High school or less", "Some college", "College +")),
             scenario = factor(case_when(
               setting == "counterfactual" ~ "counterfactual",
               setting == "standard" ~ paste0("model", policymodel),
               TRUE ~ NA), levels = c("counterfactual", "model1", 
                                      "model2", "model3", "model4")),
             !!alccatref := factor(!!alccatref, levels = c("Low risk", "Medium risk", "High risk"))) 
    
    # 1) MEAN ALCOHOL CONSUMPTION LEVELS
    
    # mean across model runs (seeds)
    output1.mean <- Output %>%
      group_by(scenario, year, sex, education, !!alccatref) %>% 
      summarise(meangpd = mean(as.numeric(meansimulation)))
    
    # min/max across model runs (seeds)
    var <- Output %>% filter(year == year_policy) %>% 
      group_by(scenario, year, sex, education, !!alccatref) %>% 
      mutate(min = min(as.numeric(meansimulation)),
             max = max(as.numeric(meansimulation)),
             var = ifelse(meansimulation == min, "max",
                          ifelse(meansimulation == max, "min", NA))) %>% ungroup() %>%
      filter(meansimulation == min | meansimulation == max) %>%
      dplyr::select(c(seed, scenario, sex, education, !!alccatref, var)) 
    
    output1 <- merge(Output, var, all.y = T) %>% 
      rename("meangpd" = "meansimulation") %>% 
      dplyr::select(c(scenario, year, sex, education, !!alccatref, meangpd, var)) %>%
      pivot_wider(names_from = "var", values_from = "meangpd") %>%
      left_join(output1.mean, .)
    
    plot1m <- ggplot(data = output1[output1$sex == "Men",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = meangpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(!!alccatref), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of alcohol per day") +
      scale_x_continuous(breaks = seq(2014, 2019, 1), limits = c(2014, 2019)) + 
      ggtheme + xlab("") + scale_color_manual(values = c5, name="") + 
      ggtitle("Simulated reduction in alcohol use", "Men")

    plot1w <- ggplot(data = output1[output1$sex == "Women",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = meangpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(!!alccatref), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of alcohol per day") +
      scale_x_continuous(breaks = seq(2014, 2019, 1), limits = c(2014, 2019)) + 
      ggtheme + xlab("") + scale_color_manual(values = c5, name="") + 
      ggtitle("", "Women")
    
    plot1 <- ggarrange(plot1m, plot1w, ncol = 1, common.legend = T, legend = "bottom")
    
    # 2) CHANGE IN ALCOHOL CONSUMPTION LEVELS
    
    counterfactual <- output1 %>% filter(scenario == "counterfactual") %>% ungroup() %>%
      rename("meangpdref" = "meangpd", "maxref" = "max", "minref" = "min") %>% 
      dplyr::select(-scenario)
    
    output2 <- output1 %>% filter(scenario != "counterfactual") %>% 
      left_join(., counterfactual) %>% 
      mutate(diffgpd = meangpd - meangpdref,
             diffmin = min - minref,
             diffmax = max - maxref,
             percgpd = (meangpd - meangpdref) / meangpdref,
             percmin = (min - minref) / minref,
             percmax = (max - maxref) / maxref) %>% 
      dplyr::select(c(scenario, year, sex, education, !!alccatref,
                      diffgpd, diffmin, diffmax,
                      percgpd, percmin, percmax)) 
    
    plot2m <- ggplot(data = output2[output2$sex == "Men",], 
                    aes(x = year, colour = scenario)) + 
      geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffgpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(2014, 2019, 1), limits = c(2014,2019)) + 
      ylab("Change in mean grams per day (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use","Men")
    
    plot2w <- ggplot(data = output2[output2$sex == "Women",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffgpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(2014, 2019, 1), limits = c(2014,2019)) + 
      ylab("Change in mean grams per day (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("","Women")

    plot2 <- ggarrange(plot2m, plot2w, ncol = 1, common.legend = T, legend = "bottom")
    
    plot3m <- ggplot(data = output2[output2$sex == "Men",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = percmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percgpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(2014, 2019, 1), limits = c(2014,2019)) + 
      scale_y_continuous(labels = scales::percent) + 
      ylab("Change in mean grams per day in % (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use", "Men")
    
    plot3w <- ggplot(data = output2[output2$sex == "Women",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = percmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percgpd), linewidth = 0.5) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(2014, 2019, 1), limits = c(2014,2019)) + 
      scale_y_continuous(labels = scales::percent) + 
      ylab("Change in mean grams per day in % (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("", "Women")

    plot3 <- ggarrange(plot3m, plot3w, ncol = 1, common.legend = T, legend = "bottom")
    
    list <- list(output1, output2, plot1, plot2, plot3)
    
  }
  
  return(list)
  
}
