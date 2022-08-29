# CRIB SHEET

#To view value labels, e.g.
val_labels(data$variablename)

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
