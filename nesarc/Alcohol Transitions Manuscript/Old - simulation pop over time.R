
# SIMAH - NESARC Alcohol Transitions
# Data Analysis


# Simulate Probabilities over time
  
# Simulate population over multiple years (over 10 years)
AlcUse_overtime <- rbind( simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 1),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 2),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 3),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 4),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 5),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 6),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 7),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 8),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 9),
                          simulate_pop(AlcUse_basepop, aTP_alc4, transition_alc4, 10))
  saveRDS(AlcUse_overtime, paste0(data, "AlcUse_overtime.RDS"))


AlcUse_overtime <- readRDS(paste0(data, "AlcUse_overtime.RDS")) %>% 
    mutate(
      # Rename and rearrange variables for the figures
      age7 = paste(age7, "years"), # Add "years" to the category label
      edu = recode(edu, "Low" = "Low Edu.", "Med" = "Medium Edu.", "High" = "High Edu."),
      AlcUse_1 = fct_relevel(AlcUse_1, "Non-drinker", "Category I", "Category II", "Category III"),
      predicted_cat = fct_relevel(predicted_cat, "Non-drinker", "Category I","Category II", "Category III"),
      age7 = fct_relevel (age7, "18-20 years", "21-25 years", "26-29 years", "30-39 years", "40-49 years", "50-64 years", "65+ years"),
      edu = fct_relevel (edu, "Low Edu.", "Medium Edu.", "High Edu."))


# AlcUse_overtime %>%
#   group_by (year, AlcUse_1, age7, edu, sex, race, predicted_cat) %>% count() %>%  ungroup() %>%
#   group_by(year, AlcUse_1, age7, edu, sex, race) %>%
#   mutate(total = sum(n), pct_total = n / total * 100) %>%
#   ungroup() %>% 
#   filter(year %in% c(1) & AlcUse_1=="Non-drinker" & predicted_cat=="Non-drinker" &
#           age7 %in% c("18-20 years", "65+ years") &
#           race=="White, non-Hispanic" & edu == "High Edu." & sex=="Men") %>% 
#   kable()




# 2.4.1) Overall Trajectories *******************************************************************************
    
      # Overall trajectories
      AlcUse_overtime %>%
        # Calculate proportions
        group_by (year, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()  %>% 
        # plot data
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        labs(x = "Time (years)", y="Proportion (%)", color="Predicted state:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))


      # Stratified by the initial state
      # Calculate proportions
      overall_traj <- AlcUse_overtime %>%
        group_by (year, AlcUse_1, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, AlcUse_1) %>%
          mutate(total = sum(n), pct_total = n / total * 100) %>%
        ungroup() 
      
      # Create dummy data for proportions at year = 0
      overall_traj_init <- overall_traj %>%
        filter(year==1) %>%
        mutate(year = 0,
               n = ifelse(AlcUse_1 == predicted_cat, total, 0),
               pct_total = ifelse(AlcUse_1 == predicted_cat, 100, 0))
      
      # Plot the proportions over time stratified by initial state and age (Figure S1)
      rbind (overall_traj_init, overall_traj) %>% 
        mutate (AlcUse_1 = paste("Initial state:", AlcUse_1),
                AlcUse_1 = fct_relevel(AlcUse_1, "Initial state: Non-drinker")) %>% 
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(~AlcUse_1) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      ggsave(paste0(output, "Figure 1a - AlcUse over time.tiff"), dpi=600, width=12, height = 4)
      

      

# 2.4.2) Age-specific Trajectories *******************************************************************************
      
      # Overall trajectories
      AlcUse_overtime %>%
        # Calculate proportions
        group_by (year, age7, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, age7) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()  %>% 
        # plot data
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(~age7) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      
      
      # Stratified by the initial state
      # Calculate proportions
      age_traj <- AlcUse_overtime %>%
        group_by (year, age7, AlcUse_1, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, age7, AlcUse_1) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()
      
      # Create dummy data for proportions at year = 0
      age_traj_init <- age_traj %>%
        filter(year==1) %>%
        mutate(year = 0,
          n = ifelse(AlcUse_1 == predicted_cat, total, 0),
          pct_total = ifelse(AlcUse_1 == predicted_cat, 100, 0))
      
      # Plot the proportions over time stratified by initial state and age (Figure S1)
      rbind (age_traj_init, age_traj) %>% 
        mutate (AlcUse_1 = paste("Initial state:", AlcUse_1),
                AlcUse_1 = fct_relevel(AlcUse_1, "Initial state: Non-drinker")) %>% 
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(age7 ~ AlcUse_1) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      ggsave(paste0(output, "Figure S1 - Age stratified AlcUse over time.tiff"), dpi=600, width=8.5, height = 11, units="in")

      
      # Proportion who start drinking for each age group
      age_traj %>%
        filter(year %in% c(1, 5) & AlcUse_1=="Non-drinker" & predicted_cat=="Non-drinker") %>% 
        rename (Age_group = age7,
                Initial_AlcUse = AlcUse_1,
                Percent_NonDrinker = pct_total) %>% 
        mutate (Percent_NonDrinker = round(Percent_NonDrinker,0), 
                Percent_Drinker = 100 - Percent_NonDrinker) %>%
        select (Age_group, year, Percent_Drinker) %>%
        kable()


# 2.4.3) Education-specific Trajectories *******************************************************************************
      
      # Overall trajectories
      AlcUse_overtime %>%
        # Calculate proportions
        group_by (year, edu, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, edu) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()  %>% 
        # plot data
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(~edu) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      
      
      
      # Stratified by the initial state
      # Calculate proportions
      edu_traj <- AlcUse_overtime %>%
        group_by (year, edu, AlcUse_1, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, edu, AlcUse_1) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()
      
      # Create dummy data for proportions at year = 0
      edu_traj_init <- edu_traj %>%
        filter(year==1) %>%
        mutate(year = 0,
          n = ifelse(AlcUse_1 == predicted_cat, total, 0),
          pct_total = ifelse(AlcUse_1 == predicted_cat, 100, 0))
      
      
      # Plot the proportions over time stratified by initial state and age (Figure S1)
      rbind (edu_traj_init, edu_traj) %>% 
        mutate (AlcUse_1 = paste("Initial state:", AlcUse_1),
                AlcUse_1 = fct_relevel(AlcUse_1, "Initial state: Non-drinker")) %>% 
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(edu ~ AlcUse_1) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      ggsave(paste0(output, "Figure S1 - Edu stratified AlcUse over time.tiff"), dpi=600, width=8.5, height = 6, units="in")


      
# 2.4.3) Race-specific Trajectories *******************************************************************************
      
      # Overall trajectories
      AlcUse_overtime %>%
        # Calculate proportions
        group_by (year, race, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, race) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()  %>% 
        # plot data
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(~race) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      
      
      
      # Stratified by the initial state
      # Calculate proportions
      race_traj <- AlcUse_overtime %>%
        group_by (year, race, AlcUse_1, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, race, AlcUse_1) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()
      
      # Create dummy data for proportions at year = 0
      race_traj_init <- race_traj %>%
        filter(year==1) %>%
        mutate(year = 0,
          n = ifelse(AlcUse_1 == predicted_cat, total, 0),
          pct_total = ifelse(AlcUse_1 == predicted_cat, 100, 0))
      
      # Plot the proportions over time stratified by initial state and age (Figure S1)
      rbind (race_traj_init, race_traj) %>% 
        mutate (AlcUse_1 = paste("Initial state:", AlcUse_1),
                AlcUse_1 = fct_relevel(AlcUse_1, "Initial state: Non-drinker")) %>% 
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(race ~ AlcUse_1) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      ggsave(paste0(output, "Figure S1 - Race stratified AlcUse over time.tiff"), dpi=600, width=8.5, height = 9, units="in")



            
# 2.4.3) Sex-specific Trajectories *******************************************************************************
      
      # Overall trajectories
      AlcUse_overtime %>%
        # Calculate proportions
        group_by (year, sex, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, sex) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()  %>% 
        # Plot data
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(~sex) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      
      
      # Stratified by the initial state
      # Calculate proportions
      sex_traj <- AlcUse_overtime %>%
        group_by (year, sex, AlcUse_1, predicted_cat) %>% count() %>%  ungroup() %>%
        group_by(year, sex, AlcUse_1) %>%
        mutate(total = sum(n),   pct_total = n / total * 100) %>%
        ungroup()
      
      # Create dummy data for proportions at year = 0
      sex_traj_init <- sex_traj %>%
        filter(year==1) %>%
        mutate(year = 0,
          n = ifelse(AlcUse_1 == predicted_cat, total, 0),
          pct_total = ifelse(AlcUse_1 == predicted_cat, 100, 0))
      
      
      # Plot the proportions over time stratified by initial state and age (Figure S1)
      rbind (sex_traj_init, sex_traj) %>% 
        mutate (AlcUse_1 = paste("Initial state:", AlcUse_1),
                AlcUse_1 = fct_relevel(AlcUse_1, "Initial state: Non-drinker")) %>% 
        ggplot(aes(x=year, y=pct_total, group=predicted_cat)) + 
        geom_line(aes(color=predicted_cat), size=1) +
        facet_grid(sex ~ AlcUse_1) + 
        labs(x = "Time (years)", y="Proportion (%)", color="State at Follow-up:") +
        theme(legend.position = "top",
          panel.grid.major=element_line(color="grey90"), 
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(linetype = "solid", fill = NA)) + 
        scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      ggsave(paste0(output, "Figure S1 - Sex stratified AlcUse over time.tiff"), dpi=600, width=8.5, height = 4, units="in")




# Predicted proportion at 5 years for specific transitions ****************************************************************
AlcUse_overtime2 %>%
  filter(year==1 | year ==5) %>%
  select(year, AlcUse_1, predicted_cat, pct_total) %>%
  mutate (pct_total = round(pct_total,1)) %>% 
  kable()





