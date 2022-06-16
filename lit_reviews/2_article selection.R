# This code is written to screen titles and abstracts of articles one by one
# Author: Charlotte Probst
# Project: SIMAH

## Load libraries
library(stringi) ## To wrap the abstract in the console window

k.file == "Complete"
if(k.file == "ReveiwerA"){
  dSUB <- read.csv("SIMAH_workplace/lit_reviews/ReviewerA_clean.csv")  
} else if (k.file == "Complete") {
  dSUB <- read.csv("SIMAH_workplace/lit_reviews/COMPLETE_clean.csv")

} else {
  print("Wrong file specified")
}

## 3 = show more 
## 0 = exclude
## 1 = include
## x = leave a comment/end screening
## a = show abstract

dSUB$TI <- as.character(dSUB$TI)
dSUB$AB <- as.character(dSUB$AB)
dSUB$COMMENT <- as.character(dSUB$COMMENT)

## IMPORTANT: k.begin needs to be updated before every screening
k.begin <- min(which(is.na(dSUB$DECISION)))
k.number.screen <- 4

for(i in k.begin:(k.begin+k.number.screen)) {
  print(stri_wrap(dSUB$TI[i] ), quote = F)
  decision.1 <- readline(prompt="Decision based on title [0/1/a/x]:")
  if (decision.1 == "a") {
    print(stri_wrap(dSUB$AB[i] ), quote = F)
    decision.1 <- readline(prompt="Decision based on abstract [0/1/x]:")
  } 
  if (decision.1 == "x") {
    decision.2 <- readline(prompt="Any comments (y/n)? ")
    if (decision.2 == "y") {
      dSUB$COMMENT[i] <- readline(prompt="Enter comment:")
      decision.1 <- readline(prompt="Now make your decision [0/1/x]:")
    } else if (!(decision.2 %in% c("y", "n"))) {
      decision.2 <- readline(prompt="Wrong answer. Do you want to comment? (y/n)?:")
      if (decision.2 == "y") {
        dSUB$COMMENT[i] <- readline(prompt="Enter comment:")
        decision.1 <- readline(prompt="Now make your decision [0/1/x]:")
      }  else {
      break
      }
      
    } else {
      break      
    } 
  } else if (!(decision.1 %in% c("0", "1"))) {
      print(paste(i, "was not decided correctly. Value out of range"))
      decision.1 <- readline(prompt="Please decide again [0/1/x]:")
    } 
  if (decision.1 == "x") {
      break
    }
  
  dSUB$DECISION[i] <- decision.1
}

write.csv(dSUB, "SIMAH_workplace/lit_reviews/COMPLETE_clean.csv", na = "", row.names = F)

if(k.file == "ReveiwerA"){
  write.csv(dSUB, "SIMAH_workplace/lit_reviews/ReviewerA_clean.csv", na = "", row.names = F)
} else if (k.file == "Complete") {
  write.csv(dSUB, "SIMAH_workplace/lit_reviews/COMPLETE_clean.csv", na = "", row.names = F)
} else {
  print("Wrong file specified")
}
