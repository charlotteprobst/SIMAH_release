WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"

OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/continuous_calibration")

implausibility <- read.csv(paste0(OutputDirectory, "/implausibility-1.csv"))

data <- read_csv(paste0(OutputDirectory, "/output-1-1.csv"))


test <- implausibility %>% filter(microsim.init.race=="WHI" & microsim.init.sex=="m" & microsim.init.education!="College" & agecat=="65+")


test <- test %>% group_by(samplenum) %>% 
  summarise(implausibility=max(maximplausibility),
            mean = mean(meanimplausibility))
