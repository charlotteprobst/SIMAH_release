#' Summarise mean alcohol consumption by policy setting 
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples

summarise_alcohol_policy <- function(Output, SelectedState){
  
  # set ggplot layout
  options(digits = 2)
  
  c4 <- c("#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  c6 <- c("#FFD679", "#BFBFBF", "#A9D8B6", "#487B79", "#1F4328", "#1482AB")
  
  ggtheme <- theme_bw() + theme(legend.position="bottom",
                                strip.background = element_rect(fill="white"),
                                panel.spacing = unit(1,"lines"),
                                text = element_text(size=20))
  
  # generate output
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
    mutate(setting = "brfss")
  
  output1 <- Output %>%
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
                            levels = c("brfss", "counterfactual", "standard", "min", "max", "mup"),
                            labels = c("BRFSS (upshifted)", "No policy",
                                       "Taxation (standard)", "Taxation (minimum)", "Taxation (maximum)",
                                       "Minimum unit price")))
  
  plot1 <- ggplot(data = output1, aes(x = year, y = meangpd, colour = setting)) + 
    geom_line(linewidth = 1) +
    facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
    ylim(0,NA) + ylab("Mean grams of alcohol per day") +
    scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(2010,2019)) + 
    ggtheme + xlab("") + 
    scale_color_manual(values = c6, name="") + 
    ggtitle("Simulated reduction in alcohol use")
  
  output2 <- output1 %>% ungroup() %>%
    #dplyr::select(-c(samplenum, seed)) %>%
    pivot_wider(names_from = setting, values_from = meangpd) %>%
    mutate(diffSta = `Taxation (standard)` - `No policy`,
           diffMin = `Taxation (minimum)` - `No policy`,
           diffMax = `Taxation (maximum)` - `No policy`,
           diffMUP = `Minimum unit price` - `No policy`,
           percSta = (`Taxation (standard)` - `No policy`) / `No policy`,
           percMin = (`Taxation (minimum)` - `No policy`) / `No policy`,
           percMax = (`Taxation (maximum)` - `No policy`) / `No policy`,
           percMUP = (`Minimum unit price` - `No policy`) / `No policy`) %>% 
    pivot_longer(cols = c(diffSta, diffMin, diffMax, diffMUP, 
                          percSta, percMin, percMax, percMUP),
                 names_to = "setting", values_to = "value") %>%
    mutate(out = substr(setting, start = 1, stop = 4),
           setting = substr(setting, start = 5, stop = 7)) %>% 
    pivot_wider(names_from = out, values_from = value) %>%
    mutate(setting = factor(setting, 
                            levels = c("Sta", "Min", "Max", "MUP"),
                             labels = c("Taxation (standard)", "Taxation (minimum)", 
                                        "Taxation (maximum)", "Minimum unit price")))
  
  plot2 <- ggplot(data = output2, aes(x = year, y = diff, colour = setting)) + 
    geom_line(linewidth = 1) +
    facet_grid(cols = vars(education), rows = vars(sex)) + 
    scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(2010,2019)) + 
    ggtheme + xlab("") + ylab("") +
    scale_color_manual(values = c4, name="") + 
    ggtitle("Simulated reduction in alcohol use", "Change in mean grams per day, reference: no policy")
  
  plot3 <- ggplot(data = output2, aes(x = year, y = perc, colour = setting)) + 
    geom_line(linewidth = 1) +
    facet_grid(cols = vars(education), rows = vars(sex)) + 
    scale_y_continuous(labels = scales::percent) + 
    scale_x_continuous(breaks = scales::pretty_breaks(), limits = c(2010,2019)) + 
    ggtheme + xlab("") + ylab("") +
    scale_color_manual(values = c4, name="") + 
    ggtitle("Simulated reduction in alcohol use", "% change in mean grams per day, reference: no policy")
  
  }

  list <- list(plot1, plot2, plot3)
  return(list)
  
}
