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
  col <- colorRampPalette(c("#A9D8B6", "#1F4328"))
  c4 <- col(4)
  c5 <- c("#BFBFBF", c4)
  c6 <- c("#FFD679", "#BFBFBF", c4)
  c2 <- c("#E7BC29", "#9C85C0")
  c4_cat <- c("#ECCB84", "#88C391", "#3B7A6D", "#162F4E")
  c3_cat <- c("#88C391", "#3B7A6D", "#162F4E")
  
  ggtheme <- theme_bw() + theme(legend.position="right",
                                strip.background = element_rect(fill="white"),
                                panel.spacing = unit(1,"lines"),
                                text = element_text(size=20))
  
  # generate output for continuous alcohol use
  
  if(output_type=="alcoholcont") {
    
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
             var = ifelse(meansimulation == min, "min",
                          ifelse(meansimulation == max, "max", NA))) %>% ungroup() %>%
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
      facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of pure alcohol per day") +
      scale_x_continuous(breaks = seq(2000, year_policy, 3), limits = c(2000,year_policy+0.1)) + 
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
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = seq(year_policy-5, year_policy, 1), limits = c(year_policy-5, year_policy+0.1)) + 
      ylab("Change in mean grams per day (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use (total population)")
    
    plot3 <- ggplot(data = output2, aes(x = year, colour = scenario)) + 
      geom_line(aes(y = percmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percgpd), linewidth = 0.5) +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = seq(year_policy-5, year_policy, 1), limits = c(year_policy-5,year_policy+0.1)) + 
      scale_y_continuous(labels = scales::percent) + 
      ylab("Change in mean grams per day in % (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use (total population)")

    list <- list(output1, output2, plot1, plot2, plot3)
    
  }
  
  # generate output for categorial alcohol use

  if(output_type=="alcoholcat") {
    
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
               setting == "standard" ~ paste0("model", policymodel),
               TRUE ~ NA), levels = c("counterfactual", "model1", 
                                      "model2", "model3", "model4"))) 
    
      # 1) MEAN PROPORTION BY ALCOHOL CATEGORY OVER TIME
      
      # mean across model runs (seeds)
      output1.mean <- Output %>%
        group_by(scenario, year, sex, education, alc_cat) %>% 
        summarise(meanprop = mean(as.numeric(propsimulation)))
      
      # min/max across model runs (seeds)
      var <- Output %>% filter(year == year_policy) %>% 
        group_by(scenario, year, sex, education, alc_cat) %>% 
        mutate(min = min(as.numeric(propsimulation)),
               max = max(as.numeric(propsimulation)),
               var = ifelse(propsimulation == min, "min",
                            ifelse(propsimulation == max, "max", NA))) %>% ungroup() %>%
        filter(propsimulation == min | propsimulation == max) %>%
        dplyr::select(c(seed, scenario, sex, education, alc_cat, var)) 
      
      output1 <- merge(Output, var, all.y = T) %>% 
        rename("meanprop" = "propsimulation") %>% 
        dplyr::select(c(scenario, year, sex, education, alc_cat, meanprop, var)) %>%
        pivot_wider(names_from = "var", values_from = "meanprop") %>%
        left_join(output1.mean, .)
      
      # non-drinkers
      plot1nd <- ggplot(data = output1[output1$alc_cat == "Non-drinker",], 
                      aes(x = year, color = scenario)) + 
        geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = meanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, 2019.5), breaks = seq(year_policy-5, 2019, 3)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.5)) +
        ggtheme + xlab("") + ylab("") +
        scale_color_manual(values = c5, name = "") + 
        ggtitle("Simulated reduction in the prevalence of non-drinkers")
    
      # low-risk
      plot1low <- ggplot(data = output1[output1$alc_cat == "Low risk",], 
                         aes(x = year, color = scenario)) + 
        geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = meanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, 2019.5), breaks = seq(year_policy-5, 2019, 3)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, .75)) +
        ggtheme + xlab("") + ylab("") +
        scale_color_manual(values = c5, name = "") + 
        ggtitle("Simulated reduction in the prevalence of low-risk drinkers")

      # medium-risk
      plot1med <- ggplot(data = output1[output1$alc_cat == "Medium risk",], 
                         aes(x = year, color = scenario)) + 
        geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = meanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, 2019.5), breaks = seq(year_policy-5, 2019, 3)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, .08)) +
        ggtheme + xlab("") + ylab("") +
        scale_color_manual(values = c5, name = "") + 
        ggtitle("Simulated reduction in the prevalence of medium-risk drinkers")
      
      # high-risk
      plot1high <- ggplot(data = output1[output1$alc_cat == "High risk",], 
                          aes(x = year, color = scenario)) + 
        geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = meanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, year_policy+0.1), breaks = seq(year_policy-5, year_policy, 1)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, .08)) +
        ggtheme + xlab("") + ylab("") +
        scale_color_manual(values = c5, name = "") + 
        ggtitle("Simulated reduction in the prevalence of high-risk drinkers")
      
      # 2) PERCENTAGE POINTS DIFFERENCE BY ALCOHOL CATEGORY
      
      counterfactual <- output1 %>% filter(scenario == "counterfactual") %>%
        rename("meanpropref" = "meanprop", "minref" = "min", "maxref" = "max") %>%
        ungroup() %>% dplyr::select(-scenario)
      
      output2 <- output1 %>% filter(scenario != "counterfactual") %>% 
        left_join(., counterfactual) %>% ungroup %>% 
        mutate(diffmeanprop = (meanprop - meanpropref)*100,
               diffmin = (min - minref)*100,
               diffmax = (max - maxref)*100,
               percmeanprop = (meanprop - meanpropref) / meanpropref,
               percmin = (min - minref) / minref,
               percmax = (max - maxref) / maxref) %>% 
        dplyr::select(c(scenario, year, sex, education, alc_cat,
                        diffmeanprop, diffmin, diffmax,
                        percmeanprop, percmin, percmax)) 
      
      # non-drinkers
      plot2nd <- ggplot(data = output2[output2$alc_cat == "Non-drinker",], 
                        aes(x = year, color = scenario)) + 
        geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmeanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, year_policy+0.1), breaks = seq(year_policy-5, year_policy, 1)) + 
        scale_y_continuous(limits = c(0, 10)) +
        ggtheme + xlab("") + ylab("Percentage points") +
        scale_color_manual(values = c4, name = "") + 
        ggtitle("Simulated change in the prevalence of non-drinkers")
      
      # low-risk
      plot2low <- ggplot(data = output2[output2$alc_cat == "Low risk",], 
                         aes(x = year, color = scenario)) + 
        geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmeanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, year_policy+0.1), breaks = seq(year_policy-5, year_policy, 1)) + 
        scale_y_continuous(limits = c(-10, 0)) +
        ggtheme + xlab("") + ylab("Percentage points") +
        scale_color_manual(values = c4, name = "") + 
        ggtitle("Simulated change in the prevalence of low-risk drinkers")
      
      # medium-risk
      plot2med <- ggplot(data = output2[output2$alc_cat == "Medium risk",], 
                         aes(x = year, color = scenario)) + 
        geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmeanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, year_policy+0.1), breaks = seq(year_policy-5, year_policy, 1)) + 
        scale_y_continuous(limits = c(-3, 1)) +
        ggtheme + xlab("") + ylab("Percentage points") +
        scale_color_manual(values = c4, name = "") + 
        ggtitle("Simulated change in the prevalence of medium-risk drinkers")
      
      # high-risk
      plot2high <- ggplot(data = output2[output2$alc_cat == "High risk",], 
                         aes(x = year, color = scenario)) + 
        geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
        geom_line(aes(y = diffmeanprop), linewidth = 0.5) +
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        scale_x_continuous(limits = c(year_policy-5, year_policy+0.1), breaks = seq(year_policy-5, year_policy, 1)) + 
        scale_y_continuous(limits = c(-3, 1)) +
        ggtheme + xlab("") + ylab("Percentage points") +
        scale_color_manual(values = c4, name = "") + 
        ggtitle("Simulated change in the prevalence of high-risk drinkers")
      
      # 3) BAR PLOT CHANGES YEARS OF POLICY
      
      output3 <- output1 %>% filter(year == year_policy | year == year_policy-1) %>%
        filter(scenario == "model4") %>%
        mutate(alc_cat = factor(alc_cat, 
                                levels = c("Non-drinker", "Low risk", "Medium risk", "High risk")))
    
      plot3 <- ggplot(output3, aes(x = as.factor(year), y = meanprop, 
                          group = alc_cat, fill = alc_cat)) +
        geom_bar(stat = "identity", position = position_stack()) + 
        facet_grid(cols = vars(education), rows = vars(sex)) + 
        ggtheme + xlab("") + ylab("") +
        scale_y_continuous(labels = scales::percent) + 
        scale_fill_manual(values = c4_cat, name = "") + 
        ggtitle("Prevalence of alcohol use categories before and after the policy", "Model 4")

      # 4) 
      
      output4 <- output1 %>% filter(year == year_policy | year == year_policy-1) %>% 
        filter(alc_cat == "Non-drinker")
        
      plot4 <- ggplot(output4, aes(x = as.factor(year), y = meanprop, 
                                   group = scenario, color = scenario)) +
        geom_line() + geom_point(shape = 18, size = 3) + 
        facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
        ggtheme + xlab("") + ylab("") +
        scale_y_continuous(labels = scales::percent, limits = c(0,NA)) + 
        scale_color_manual(values = c5, name = "") + 
        ggtitle("Prevalence of non-drinkers in total population")
      
    list <- list(output1, plot1nd, plot1low, plot1med, plot1high,
                 plot2nd, plot2low, plot2med, plot2high,
                 plot3, plot4)
    
  }
  
  # generate output for continuous alcohol use by constant alcohol category
  
  if(output_type=="alcoholcontcat") {
    
    alccatref <- sym(paste0("alc_cat_", year_policy-1))
    
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
      geom_point(aes(y = meangpd), shape = 18, size = 3) +
      facet_grid(cols = vars(education), rows = vars(!!alccatref), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of pure alcohol per day") +
      scale_x_continuous(breaks = seq(year_policy-1, year_policy, 1), limits = c(year_policy-1, year_policy+0.1)) + 
      ggtheme + xlab("") + scale_color_manual(values = c5, name="") + 
      ggtitle("Simulated reduction in alcohol use", "Men")

    plot1w <- ggplot(data = output1[output1$sex == "Women",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = min), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = max), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = meangpd), linewidth = 0.5) +
      geom_point(aes(y = meangpd), shape = 18, size = 3) +
      facet_grid(cols = vars(education), rows = vars(!!alccatref), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of pure alcohol per day") +
      scale_x_continuous(breaks = seq(year_policy-1, year_policy, 1), limits = c(year_policy-1, year_policy+0.1)) + 
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
      geom_point(aes(y = diffgpd), shape = 18, size = 3) +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(year_policy-1, year_policy, 1), limits = c(year_policy-1, year_policy+0.1)) + 
      ylab("Change in mean grams per day (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use","Men")
    
    plot2w <- ggplot(data = output2[output2$sex == "Women",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = diffmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = diffgpd), linewidth = 0.5) +
      geom_point(aes(y = diffgpd), shape = 18, size = 3) +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(year_policy-1, year_policy, 1), limits = c(year_policy-1, year_policy+0.1)) + 
      ylab("Change in mean grams per day (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("","Women")

    plot2 <- ggarrange(plot2m, plot2w, ncol = 1, common.legend = T, legend = "bottom")
    
    plot3m <- ggplot(data = output2[output2$sex == "Men",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = percmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percgpd), linewidth = 0.5) +
      geom_point(aes(y = percgpd), shape = 18, size = 3) +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(year_policy-1, year_policy, 1), limits = c(year_policy-1, year_policy+0.1)) + 
      scale_y_continuous(labels = scales::percent) + 
      ylab("Change in mean grams per day in % (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use", "Men")
    
    plot3w <- ggplot(data = output2[output2$sex == "Women",], 
                     aes(x = year, colour = scenario)) + 
      geom_line(aes(y = percmin), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percmax), linewidth = 0.5, alpha = 0.5) + 
      geom_line(aes(y = percgpd), linewidth = 0.5) +
      geom_point(aes(y = percgpd), shape = 18, size = 3) +
      facet_grid(cols = vars(education), rows = vars(!!alccatref)) + 
      scale_x_continuous(breaks = seq(year_policy-1, year_policy, 1), limits = c(year_policy-1, year_policy+0.1)) + 
      scale_y_continuous(labels = scales::percent) + 
      ylab("Change in mean grams per day in % (reference: no policy)\n") + xlab("") + 
      ggtheme + scale_color_manual(values = c4, name="") + 
      ggtitle("", "Women")

    plot3 <- ggarrange(plot3m, plot3w, ncol = 1, common.legend = T, legend = "bottom")
    
    # 3) DETAILED CHANGE IN ALC LEVELS MODEL 4 
    
    output3 <- output2 %>% 
      filter(scenario == "model4") 
    
    plot4 <- ggplot(output3, aes(x = as.factor(year), y = diffgpd, 
                                 group = alc_cat_2018, color = alc_cat_2018)) +
      geom_line() + geom_point(shape = 18, size = 3) + 
      facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
      ggtheme + xlab("") + ylab("Grams of pure alcohol per day") +
      scale_color_manual(values = c3_cat, name = "") + 
      ggtitle("Change in average alcohol consumption levels by alcohol use categories", "Model 4")
    
    list <- list(output1, output2, plot1, plot2, plot3, plot4)
    
  }
  
  return(list)
  
}
