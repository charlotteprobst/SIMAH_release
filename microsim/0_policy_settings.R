# SIMAH project - script for setting up policy model settings

# general policy settings
policy <- 1
year_policy <- 2019
policy_int <- "price"

# define one (1) reference scenario and four (4) price policy modelling scenarios specific to beer, wine, spirits
scenarios <- cbind(policymodel = 1:5,
                   scenario = c("0,0,0", # Reference scenario
                                "0.1,0.1,0.1", # Scenario 1: uniform price increase of 10%
                                "0.3,0.3,0.3", # Scenario 2: uniform price increase of 30%
                                "0.3,0.1,0.3", # Scenario 3: beverage-specific price increases of 30% for beer and spirits and 10% for wine
                                "0.5,0.1,0.5")) %>% as.data.frame() # Scenario 4: beverage-specific price increases of 50% for beer and spirits and 10% for wine

######################DO NOT EDIT BELOW HERE ##################################################

# read in beverage-specific consumption elasticities (i.e., how average consumption levels change given higher retail prices) and 
# three (3) settings for the association between consumption levels and individual-level price elasticities as well as participation elasticies (i.e., how many people may stop drinking alcohol entirely given higher retail prices)
    # setting 1: main analysis, assuming a U-shaped association between consumption levels and individual-level price elasticities at a medium correlation (r=0·60) and participation elasticies "switched on"
    # setting 2: more conservative sensitvity analysis, assuming a higher correlation (r=0·80) and no participation elasticies 
    # setting 3: less conservative sensitvity analysis, assuming a lower correlation (r=0·40) and participation elasticies "switched on"

policy_setting <- read.csv(paste0(DataDirectory,"input_PricePolicy_sim.csv")) %>% 
  filter(policy %in% policy_int) %>% 
  mutate(beverage = as.factor(beverage))


