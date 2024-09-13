#' Summarise mean alcohol consumption by policy setting 
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples

summarise_alcohol_policy <- function(Output, SelectedState, out = main){
  
  # set ggplot layout
  options(digits = 4)
  
  c4 <- c("#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  c6 <- c("#FFD679", "#BFBFBF", "#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  
  ggtheme <- theme_bw() + theme(legend.position="right",
                                strip.background = element_rect(fill="white"),
                                panel.spacing = unit(1,"lines"),
                                text = element_text(size=20))
  
  # generate output for continuous alcohol use
  if(output=="alcoholcont") {
    
  targetdata <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2022_final.RDS") %>%
    filter(State==SelectedState) %>% filter(YEAR>=2000 & YEAR<2020) %>%
    filter(drinkingstatus==1) %>% 
    mutate(age_cat = cut(age_var,
                         breaks=c(0,24,64,100),
                         labels=c("18-24","25-64","65+")),
           sex = ifelse(sex_recode == "Male", "Men", "Women")) %>%
    rename(year = YEAR,
           education = education_summary,
           race = race_eth) %>%
    group_by(year, sex, education) %>%
    summarise(meangpd = mean(gramsperday_upshifted)) %>%
    mutate(scenario = "brfss")
  
  if(out=="main") {
    
    output1 <- Output %>%
      mutate(sex = ifelse(sex=="m","Men","Women"),
             year = as.numeric(as.character(year)),
             scenario = case_when(
               setting == "counterfactual" ~ "counterfactual",
               scenario != 0 & setting == "standard" ~ paste0("price ", scenario, "%"),
               scenario != 0 & setting == "mup" ~ "mup",
               TRUE ~ NA)) %>%
      group_by(scenario, year, alc_cat, sex, education) %>% 
      summarise(meangpd = mean(as.numeric(meansimulation))) %>%
      #rbind(., targetdata) %>%
      mutate(year = as.numeric(as.character(year)),
             #samplenum = as.numeric(as.character(samplenum)),
             education = factor(education, 
                                levels = c("LEHS", "SomeC", "College"),
                                labels = c("High school or less", "Some college", "College +")),
             scenario = factor(scenario, 
                               levels = c("counterfactual", "price 5%", 
                                          "price 10%", "price 20%", "mup"),
                               labels = c("No policy", "5% price increase", 
                                          "10% price increase", "20% price increase", "Minimum unit price")),
             alc_cat = factor(alc_cat,
                              levels = c("Low risk","Medium risk", "High risk")))
    
    plot1A <- ggplot(data = output1[output1$alc_cat == "High risk",], aes(x = year, y = meangpd, colour = scenario)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of alcohol per day") +
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + 
      scale_color_manual(values = c6, name="") + 
      ggtitle("Simulated reduction in alcohol use — High risk alcohol user")

    plot1B <- ggplot(data = output1[output1$alc_cat == "Medium risk",], aes(x = year, y = meangpd, colour = scenario)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of alcohol per day") +
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + 
      scale_color_manual(values = c6, name="") + 
      ggtitle("Simulated reduction in alcohol use — Medium risk alcohol user")
    
    plot1C <- ggplot(data = output1[output1$alc_cat == "Low risk",], aes(x = year, y = meangpd, colour = scenario)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of alcohol per day") +
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + 
      scale_color_manual(values = c6, name="") + 
      ggtitle("Simulated reduction in alcohol use — Low risk alcohol user")
    
    output2 <- output1 %>% ungroup() %>%
      #dplyr::select(-c(samplenum, seed)) %>%
      pivot_wider(names_from = scenario, values_from = meangpd) %>%
      mutate(diffP05 = `5% price increase` - `No policy`,
             diffP10 = `10% price increase` - `No policy`,
             diffP20 = `20% price increase` - `No policy`,
             diffMUP = `Minimum unit price` - `No policy`,
             percP05 = (`5% price increase` - `No policy`) / `No policy`,
             percP10 = (`10% price increase` - `No policy`) / `No policy`,
             percP20 = (`20% price increase` - `No policy`) / `No policy`,
             percMUP = (`Minimum unit price` - `No policy`) / `No policy`) %>% 
      dplyr::select(c(year, alc_cat, sex, education,
                      diffP05, diffP10, diffP20, diffMUP, 
                      percP05, percP10, percP20, percMUP)) %>% 
      pivot_longer(cols = c(diffP05, diffP10, diffP20, diffMUP, 
                            percP05, percP10, percP20, percMUP),
                   names_to = "scenario", values_to = "value") %>%
      mutate(out = substr(scenario, start = 1, stop = 4),
             scenario = substr(scenario, start = 5, stop = 7)) %>% 
      pivot_wider(names_from = out, values_from = value) %>%
      mutate(scenario = factor(scenario, 
                              levels = c("P05", "P10", "P20", "MUP"),
                               labels = c("5% price increase", "10% price increase", 
                                          "20% price increase", "Minimum unit price")))
    
    plot2A <- ggplot(data = output2[output2$alc_cat == "High risk",], aes(x = year, y = diff, colour = scenario)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + ylab("") +
      scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use — High risk alcohol user", 
              "Change in mean grams per day, reference: no policy")
    
    plot2B <- ggplot(data = output2[output2$alc_cat == "Medium risk",], aes(x = year, y = diff, colour = scenario)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + ylab("") +
      scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use — Medium risk alcohol user", 
              "Change in mean grams per day, reference: no policy")

    plot2C <- ggplot(data = output2[output2$alc_cat == "Low risk",], aes(x = year, y = diff, colour = scenario)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + ylab("") +
      scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use — Low risk alcohol user", 
              "Change in mean grams per day, reference: no policy")
    
plot3A <- ggplot(data = output2[output2$alc_cat == "High risk",], aes(x = year, y = perc, colour = scenario)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_y_continuous(labels = scales::percent) + 
      scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
      ggtheme + xlab("") + ylab("") +
      scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use — High risk alcohol user", 
              "% change in mean grams per day, reference: no policy")

plot3B <- ggplot(data = output2[output2$alc_cat == "Medium risk",], aes(x = year, y = perc, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — Medium risk alcohol user", 
          "% change in mean grams per day, reference: no policy")

plot3C <- ggplot(data = output2[output2$alc_cat == "Low risk",], aes(x = year, y = perc, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — Low risk alcohol user", 
          "% change in mean grams per day, reference: no policy")

  }
  
  if(out=="sens") {
    
    output1 <- Output %>%
      filter(scenario == out_scenario & setting != "mup" | scenario == 0) %>%
      mutate(sex = ifelse(sex=="m","Men","Women"),
             year = as.numeric(as.character(year)),
             setting = ifelse(scenario != 0, setting,
                              ifelse(scenario == 0, "counterfactual", NA))) %>%
      group_by(setting, year, sex, education) %>% 
      summarise(meangpd = mean(meansimulation)) %>%
      rbind(., targetdata) %>%
      mutate(year = as.numeric(as.character(year)),
             #samplenum = as.numeric(as.character(samplenum)),
             education = factor(education, 
                                levels = c("LEHS", "SomeC", "College"),
                                labels = c("High school or less", "Some college", "College +")),
             setting = factor(setting, 
                              levels = c("brfss", "counterfactual", "standard", "min", "max"),
                              labels = c("BRFSS (upshifted)", "No policy",
                                         "Taxation (standard)", "Taxation (minimum)", "Taxation (maximum)")))
    
    plot1 <- ggplot(data = output1, aes(x = year, y = meangpd, colour = setting)) + 
      geom_line(linewidth = 1) + 
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
      ylim(0,NA) + ylab("Mean grams of alcohol per day") +
      scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(2010,2020)) + 
      ggtheme + xlab("") + 
      scale_color_manual(values = c6, name="") + 
      ggtitle("Simulated reduction in alcohol use")
    
    output2 <- output1 %>% ungroup() %>%
      #dplyr::select(-c(samplenum, seed)) %>%
      pivot_wider(names_from = setting, values_from = meangpd) %>%
      mutate(diffSta = `Taxation (standard)` - `No policy`,
             diffMin = `Taxation (minimum)` - `No policy`,
             diffMax = `Taxation (maximum)` - `No policy`,
             percSta = (`Taxation (standard)` - `No policy`) / `No policy`,
             percMin = (`Taxation (minimum)` - `No policy`) / `No policy`,
             percMax = (`Taxation (maximum)` - `No policy`) / `No policy`) %>% 
      pivot_longer(cols = c(diffSta, diffMin, diffMax, 
                            percSta, percMin, percMax),
                   names_to = "setting", values_to = "value") %>%
      mutate(out = substr(setting, start = 1, stop = 4),
             setting = substr(setting, start = 5, stop = 7)) %>% 
      pivot_wider(names_from = out, values_from = value) %>%
      mutate(setting = factor(setting, 
                              levels = c("Sta", "Min", "Max"),
                              labels = c("Taxation (standard)", "Taxation (minimum)", 
                                         "Taxation (maximum)")))
    
    plot2 <- ggplot(data = output2, aes(x = year, y = diff, colour = setting)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(2010,2020)) + 
      ggtheme + xlab("") + ylab("") +
      scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use", "Change in mean grams per day, reference: no policy")
    
    plot3 <- ggplot(data = output2, aes(x = year, y = perc, colour = setting)) + 
      geom_line(linewidth = 1) +
      geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
      facet_grid(cols = vars(education), rows = vars(sex)) + 
      scale_y_continuous(labels = scales::percent) + 
      scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(2010,2020)) + 
      ggtheme + xlab("") + ylab("") +
      scale_color_manual(values = c4, name="") + 
      ggtitle("Simulated reduction in alcohol use", "% change in mean grams per day, reference: no policy")
  
    }
  
  list <- list(output1, output2, plot1, plot2, plot3)
  
  }

  if(output=="alcoholcat") {
    
    targetdata <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2022_final.RDS") %>%
      filter(State==SelectedState) %>% filter(YEAR>=2000 & YEAR<2020) %>%
      mutate(age_cat = cut(age_var,
                           breaks=c(0,24,64,100),
                           labels=c("18-24","25-64","65+")),
             sex = ifelse(sex_recode == "Male", "Men", "Women"),
             alc_cat = ifelse(sex=="Men" & gramsperday_upshifted>0 &
                                gramsperday_upshifted<=40, "Low risk",
                              ifelse(sex=="Women" & gramsperday_upshifted>0 &
                                       gramsperday_upshifted<=20, "Low risk",
                                     ifelse(sex=="Men" & gramsperday_upshifted>40 &
                                              gramsperday_upshifted<=60, "Medium risk",
                                            ifelse(sex=="Women" & gramsperday_upshifted>20 &
                                                     gramsperday_upshifted<=40, "Medium risk",
                                                   ifelse(sex=="Men" & gramsperday_upshifted>60,
                                                          "High risk",
                                                          ifelse(sex=="Women" & gramsperday_upshifted>40,
                                                                 "High risk",
                                                                 ifelse(gramsperday_upshifted==0, "Non-drinker",NA)))))))) %>%
      rename(year = YEAR,
             education = education_summary,
             race = race_eth) %>%
      group_by(year, sex, education, alc_cat, .drop=FALSE) %>% tally() %>%
      ungroup() %>%
      group_by(year, sex, education) %>%
      mutate(prop=n/sum(n)) %>%
      dplyr::select(-n) %>%
      mutate(scenario = "brfss", setting = "brfss")
    
    if(out=="main") {
      
      output1 <- Output %>%
        filter(setting == "standard" | setting == "mup") %>%
        mutate(sex = ifelse(sex=="m","Men","Women"),
               year = as.numeric(as.character(year)),
               scenario = case_when(
                 scenario == 0 ~ "counterfactual",
                 scenario != 0 & setting == "standard" ~ paste0("price ", scenario, "%"),
                 scenario != 0 & setting == "mup" ~ "mup",
                 TRUE ~ NA)) %>%
        group_by(scenario, year, sex, education, alc_cat) %>% 
        summarise(prop = mean(prop)) %>%
        rbind(., targetdata) %>%
        mutate(year = as.numeric(as.character(year)),
               #samplenum = as.numeric(as.character(samplenum)),
               education = factor(education, 
                                  levels = c("LEHS", "SomeC", "College"),
                                  labels = c("High school or less", "Some college", "College +")),
               scenario = factor(scenario, 
                                 levels = c("brfss", "counterfactual", "price 5%", 
                                            "price 10%", "price 20%", "mup"),
                                 labels = c("BRFSS (upshifted)", "No policy", "5% price increase", 
                                            "10% price increase", "20% price increase", "Minimum unit price")),
               alc_cat = factor(alc_cat, 
                                levels = c("High risk", "Medium risk", "Low risk", "Non-drinker")))
      
      plot1A <- ggplot(data = output1[output1$sex == "Men",], 
                      aes(x = year, y = prop, fill = alc_cat)) + 
        geom_bar(stat = "identity") +
        facet_grid(cols = vars(education), rows = vars(scenario), scales = "free") + 
        scale_x_continuous(breaks = seq(2000, 2015, 5)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.00001)) +
        ggtheme + xlab("") + ylab("Population in alcohol category (%)") +
        scale_fill_manual(values = c4, name = "", guide = guide_legend(reverse = TRUE)) + 
        ggtitle("Simulated reduction in alcohol use", "Men")
  
      plot1B <- ggplot(data = output1[output1$sex == "Women",], 
                       aes(x = year, y = prop, fill = alc_cat)) + 
        geom_bar(stat = "identity") +
        facet_grid(cols = vars(education), rows = vars(scenario), scales = "free") + 
        scale_x_continuous(breaks = seq(2000, 2015, 5)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.00001)) +
        ggtheme + xlab("") + ylab("") +
        scale_fill_manual(values = c4, name = "", guide = guide_legend(reverse = TRUE)) + 
        ggtitle("", "Women")

    }
    
    if(out=="sens") {
      
      output1 <- Output %>%
        filter(scenario == out_scenario & setting != "mup" | scenario == 0) %>%
        mutate(sex = ifelse(sex=="m","Men","Women"),
               year = as.numeric(as.character(year)),
               setting = ifelse(scenario != 0, setting,
                                ifelse(scenario == 0, "counterfactual", NA))) %>%
        group_by(setting, year, sex, education, alc_cat) %>% 
        summarise(prop = mean(prop)) %>%
        rbind(., targetdata) %>%
        mutate(year = as.numeric(as.character(year)),
               #samplenum = as.numeric(as.character(samplenum)),
               education = factor(education, 
                                  levels = c("LEHS", "SomeC", "College"),
                                  labels = c("High school or less", "Some college", "College +")),
               setting = factor(setting, 
                                levels = c("brfss", "counterfactual", "standard", "min", "max"),
                                labels = c("BRFSS (upshifted)", "No policy",
                                           "Taxation (standard)", "Taxation (minimum)", "Taxation (maximum)")),
               alc_cat = factor(alc_cat, 
                                levels = c("High risk", "Medium risk", "Low risk", "Non-drinker")))
      
      plot1A <- ggplot(data = output1[output1$sex == "Men",], 
                       aes(x = year, y = prop, fill = alc_cat)) + 
        geom_bar(stat = "identity") +
        facet_grid(cols = vars(education), rows = vars(setting), scales = "free") + 
        scale_x_continuous(breaks = seq(2000, 2015, 5)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.00001)) +
        ggtheme + xlab("") + ylab("Population in alcohol category (%)") +
        scale_fill_manual(values = c4, name = "", guide = guide_legend(reverse = TRUE)) + 
        ggtitle("Simulated reduction in alcohol use", "Men")
      
      plot1B <- ggplot(data = output1[output1$sex == "Women",], 
                       aes(x = year, y = prop, fill = alc_cat)) + 
        geom_bar(stat = "identity") +
        facet_grid(cols = vars(education), rows = vars(setting), scales = "free") + 
        scale_x_continuous(breaks = seq(2000, 2015, 5)) + 
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.00001)) +
        ggtheme + xlab("") + ylab("") +
        scale_fill_manual(values = c4, name = "", guide = guide_legend(reverse = TRUE)) + 
        ggtitle("", "Women")
      
    }
          
    library(ggpubr)
    plot1 <- ggarrange(plot1A, plot1B, ncol = 2, common.legend = T, legend = "bottom")
    
    list <- list(output1, plot1)
    
  }

  return(list)
  
}
