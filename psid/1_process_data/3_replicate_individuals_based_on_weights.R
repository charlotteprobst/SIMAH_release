# Script to replicate individuals according to their person weights

library(tidyverse)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

df <- read_csv("SIMAH_workplace/PSID/cleaned data/all_data_1999_2021_excl_non_responders081123.csv")

# Calculate the total number of replicates needed
total_replicates <- round(sum(df$weight))

# Replicate individuals based on survey weights
replicated_df <- df %>%
  mutate(replicates = round(weight)) %>%
  uncount(replicates)

# View the replicated dataframe
print(replicated_df)