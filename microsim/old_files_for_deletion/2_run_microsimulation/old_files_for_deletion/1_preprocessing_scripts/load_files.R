###loading files to be used in microsimulation

# READ IN MICROSIM AND MIGRANT FILES




####PREP MICROSIM OUTPUTS
PopPerYear <- list()
DeathSummary <- list()

# save a copy of original population files
baseorig <- basepop

# set microsim individuals IDs 
basepop <- cbind(1:nrow(basepop), basepop)
