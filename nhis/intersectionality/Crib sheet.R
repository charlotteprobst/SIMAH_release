# CRIB SHEET

#To view value labels, e.g.
val_labels(data$variablename)

# val_labels(nhis_subset1_raw$ALCSTAT1)
# 0  NIU  
# 1  Lifetime abstainer(lt 12 drinks in life)                                         
# 2  Former drinker (no drinks past year) 
# 3  Current drinker (1+ drinks past year)                                          
# 9  Drinking status unknown
# 
# val_labels(nhis_subset1_raw$ALCSTAT2)
# NIU                                     Lifetime abstainer 
# 0                                       10 
# Former drinker                          Former infrequent drinker 
# 20                                      21 
# Former regular drinker                  Former drinker, unknown frequency 
# 22                                      23 
# Current drinker                         Current infrequent drinker 
# 30                                      31 
# Current light drinker                   Current moderate drinker 
# 32                                      33 
# Current heavy drinker                   Current drinker, level unknown 
# 34                                      35 
# Unknown-refused                         Unknown-don't know 
# 97                                            99 


# To get a summary of a column's counts
data %>% count(columnname)
data %>% count(NHISPID) # To know number of individuals

# Using count and starts with together:
count(df, across(starts_with("ALC")))

# If else statements:
if (x < 0) {
  print("Negative number")
} else if (x > 0) {
  print("Positive number")
} else
  print("Zero")

# Checking face validity by subsetting data:
nhis_subset1_grams %>% group_by(ALCSTAT1) %>% count()

## Recode a labelled dataframe as factors
df <- haven::as_factor(df)
