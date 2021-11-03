#script for IPF 

# read in constraints 
cons <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraints_IPF_final.csv") %>% 
  filter(STATE==SelectedState) %>% 
  dplyr::select(-STATE)
# select the appropriate state 
percentpop <- PopulationSize / rowSums(cons)
percentpop <- ifelse(percentpop>1, 1, percentpop)

# if one of the big states or USA - use 30% of the constraints (otherwise CPU overload)
if(SelectedState=="USA" | SelectedState=="California" | SelectedState=="Texas"){
cons <- cons*0.3
}

##reading in and processing individual level data
###read in BRFSS data 
source("SIMAH_code/microsim/1_generate_baseline_population/scripts/BRFSS_processing.R")

##tidy up
gc()
rm(list=setdiff(ls(), c("cons", "brfss", c(tokeep))))
##make sure constraints are in alphabetical order

cons <- data.frame(cons)
cons <- cons[ , order(names(cons))]

cat_labs <- names(cons)

###organise individual-level data for IPF 

brfss <- data.frame(brfss)
##save copy 
brfss.raw <- brfss

##get vars needed for IPF 
individual <- data.frame(RACE=brfss$RACE, agecat=brfss$agecat, SEX=brfss$SEX,EDUCATION=brfss$EDUCATION)

##Code from Lovelace book for IPF 

###make binary race/age/sex
ASR <- paste0(individual$RACE, individual$SEX, individual$agecat, individual$EDUCATION)

##turn individual level data into binary - ageracesex
m1 <- model.matrix(~ASR-1)
colnames(m1) <- sub("ASR", "", colnames(m1))

###create binary data file 
ind_cat <- as.data.frame(m1)
rm(m1)
gc()

cat_labs
ind_cat <- ind_cat[ , order(names(ind_cat))]
cons <- cons[, order(names(cons))]
cons <- cons[,c(names(ind_cat))]
colnames(cons) == names(ind_cat)
names(ind_cat) == colnames(cons)
setdiff( names(ind_cat),colnames(cons))
setdiff(names(cons),names(ind_cat))

#create an empty weight matrix with the right dimensions
weights <- array(NA, dim=c(nrow(individual),nrow(cons))) 
ind_agg <- matrix(colSums(ind_cat), nrow(cons), ncol(cons), byrow = T)
cons <- apply(cons, 2, as.numeric) # convert the constraints to 'numeric'
ind_catt <- t(ind_cat) # transpose the dummy variables for ipfp
x0 <- rep(1, nrow(individual)) # set the initial weights - 1 
options(scipen=999)
##IPF to generate new weights - should print v small numbers e-05 etc.
weights <- ipfp(cons, ind_catt, x0, maxit=100, v=T)

ind_agg <- colSums(weights*ind_cat)

###maximum absolute difference in any category
max(abs(ind_agg-cons))

## Integerisation ##
source("SIMAH_code/microsim/1_generate_baseline_population/scripts/Lovelace_functions.R")

##using robin lovelace code - truncate replicate sample method of integerisation

ints <- int_expand_vector(int_trs(weights))
ints_df <- data.frame(id = ints)
brfss.raw$id <- 1:nrow(brfss.raw) # assign each individual an id##join the simulated data with the individual level data 
gc()
ints_df <- inner_join(ints_df, brfss.raw)

###postprocessing - names of variables etc.
microsim <- data.frame(microsim.init.id=1:nrow(ints_df), 
                       microsim.init.sex=ints_df$SEX, 
                       microsim.init.age=ints_df$age_var, 
                       microsim.init.race=ints_df$RACE, 
                       microsim.roles.employment.status=ints_df$EMPLOYED, 
                       microsim.init.education=ints_df$EDUCATION, 
                       microsim.init.BMI=ints_df$BMI, 
                       microsim.init.income=ints_df$household_income, 
                       microsim.init.drinkingstatus=ints_df$drinkingstatus, 
                       microsim.init.alc.gpd=ints_df$gramsperday, 
                       agecat=ints_df$agecat, 
                       alcdays=ints_df$frequency,
                       formerdrinker = ints_df$formerdrinker)

rm(list=setdiff(ls(), c("microsim", "cons", c(tokeep))))

####sample to get desired population size 
if(percentpop<1){
microsim <- sample_n(microsim, PopulationSize, replace=F)
}

#  script to sample age to ensure the correct age distribution when wide cats used
source("SIMAH_code/microsim/1_generate_baseline_population/scripts/samplingages.R")

microsim$microsim.init.id <- 1:nrow(microsim)
