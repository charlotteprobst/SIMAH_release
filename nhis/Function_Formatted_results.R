

## Functions for aalen interaction models to extract formatted results



# Function to extract results from Aalen model, multiple by 10,000py and round/format results
aalen_10000py <- function(model, x) {
  mu <- model$gamma[x]       
  var <-  model$var.gamma[x,x]
  confint.lower <- round((mu - (1.96 * sqrt(var)))*10000,1) # CI * 10,000 to get result per 10,000py
  confint.upper <- round((mu + (1.96 * sqrt(var)))*10000,1) # CI * 10,000 to get result per 10,000py
  mu <- round(mu*10000,1)                                   # mu * 10,000 to get result per 10,000py 
  output<-paste0(mu, " (",confint.lower,", ", confint.upper, ")")
  return(cat(output, "\n"))}   #cat() returns the text without quotes and without the leading numbers [1], [2]...




# Function to extract results from Aalen model, multiple by 10,000py, for a 10-unit increase, and round/format results
aalen_10000py_10x <- function(model, x) {
  mu <- model$gamma[x]       
  var <-  model$var.gamma[x,x]
  confint.lower <- round((mu - (1.96 * sqrt(var)))*10000*10,1) # CI * 10,000 to get result per 10,000py x 10 unit increase
  confint.upper <- round((mu + (1.96 * sqrt(var)))*10000*10,1) # CI * 10,000 to get result per 10,000py x 10 unit increase
  mu <- round(mu*10000*10,1)                                   # mu * 10,000 to get result per 10,000py x 10 unit increase
  output<-paste0(mu, " (",confint.lower,", ", confint.upper, ")")
  return(cat(output, "\n"))}   #cat() returns the text without quotes and without the leading numbers [1], [2]...

