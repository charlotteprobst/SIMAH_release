# Convert missing data for 'refused', 'not ascertained', 'don't know', 'inconsistent' and 'NIU' from all columns

convert_missing_data_NA <- function(data){
  
  # Specify values for missing variables 
  data$AGE[data$AGE==997|data$AGE==998|data$AGE==999] <- NA
  data$SEX[data$SEX==7|data$SEX==8|data$SEX==9] <- NA
  data$SEXORIEN[data$SEXORIEN==7|data$SEXORIEN==8] <- NA
  data$EDUCREC2[data$EDUCREC2==96|data$EDUCREC2==97|data$EDUCREC2==98|data$EDUCREC2==99] <- NA
  data$RACENEW[data$RACENEW==997|data$RACENEW==998|data$RACENEW==999] <- NA
  data$USBORN[data$USBORN==96|data$USBORN==97|data$USBORN==98|data$USBORN==99] <- NA
  data$CITIZEN[data$CITIZEN==7|data$CITIZEN==8|data$CITIZEN==9] <- NA
  data$INCFAM97ON2[data$INCFAM97ON2==97|data$INCFAM97ON2==98|data$INCFAM97ON2==99] <- NA
  data$ALCDRINKEV[data$ALCDRINKEV==7|data$ALCDRINKEV==8|data$ALCDRINKEV==9] <- NA
  data$ALCANYNOE[data$ALCANYNOE==0|data$ALCANYNOE==996|data$ALCANYNOE==997|data$ALCANYNOE==998|data$ALCANYNOE==999] <- NA
  data$ALCANYTPE[data$ALCANYTPE==6|data$ALCANYTPE==7|data$ALCANYTPE==8|data$ALCANYTPE==9] <- NA
  data$ALC5UPEVYR[data$ALC5UPEVYR==7|data$ALC5UPEVYR==8|data$ALC5UPEVYR==9] <- NA
  data$ALCEV30D[data$ALCEV30D==0|data$ALCEV30D==7|data$ALCEV30D==8|data$ALCEV30D==9] <- NA
  data$ALC5UPOCC30D[data$ALC5UPOCC30D==0|data$ALC5UPOCC30D==97|data$ALC5UPOCC30D==98|data$ALC5UPOCC30D==99] <- NA
  data$ALCDRKHVY12M[data$ALCDRKHVY12M==0|data$ALCDRKHVY12M==7|data$ALCDRKHVY12M==8|data$ALCDRKHVY12M==9] <- NA
  data$ALC1YR[data$ALC1YR==0|data$ALC1YR==7|data$ALC1YR==8|data$ALC1YR==9] <- NA
  data$ALCLIFE[data$ALCLIFE==0|data$ALCLIFE==7|data$ALCLIFE==8|data$ALCLIFE==9] <- NA
  data$ALCAMT[data$ALCAMT==0|data$ALCAMT==96|data$ALCAMT==97|data$ALCAMT==98|data$ALCAMT==99] <- NA
  data$ALC5UPYR[data$ALC5UPYR==0 | data$ALC5UPYR==996 |data$ALC5UPYR==997 | data$ALC5UPYR==998 | data$ALC5UPYR==999] <- NA
  data$ALCSTAT1[data$ALCSTAT1==0|data$ALCSTAT1==9] <- NA
  data$ALCSTAT2[data$ALCSTAT2==0|data$ALCSTAT2==97|data$ALCSTAT2==99] <- NA
  data$ALCANYNO[data$ALCANYNO==0|data$ALCANYNO==996|data$ALCANYNO==997|data$ALCANYNO==998|data$ALCANYNO==999] <- NA
  data$ALCANYTP[data$ALCANYTP==0|data$ALCANYTP==6|data$ALCANYTP==7|data$ALCANYTP==8|data$ALCANYTP==9] <- NA
  data$ALCDAYSMO[data$ALCDAYSMO==96|data$ALCDAYSMO==97|data$ALCDAYSMO==98|data$ALCDAYSMO==99] <- NA
  data$ALCDAYSWK[data$ALCDAYSWK==95|data$ALCDAYSWK==96|data$ALCDAYSWK==97|data$ALCDAYSWK==98|data$ALCDAYSWK==99] <- NA
  data$ALCDAYSYR[data$ALCDAYSYR==995|data$ALCDAYSYR==996|data$ALCDAYSYR==997|data$ALCDAYSYR==998|data$ALCDAYSYR==999] <- NA
  data$ALC5UPNO[data$ALC5UPNO==996|data$ALC5UPNO==997|data$ALC5UPNO==998|data$ALC5UPNO==999] <- NA
  data$ALC5UPTP[data$ALC5UPTP==0|data$ALC5UPTP==7|data$ALC5UPTP==8|data$ALC5UPTP==9] <- NA
  data$MORTSTAT[data$MORTSTAT==9] <- NA
  data$MORTDODQ[data$MORTDODQ==9] <- NA
  data$MORTDODQ[data$MORTDODY==9999] <- NA
  data$MORTUCOD[data$MORTUCOD==999] <- NA
  data$MORTUCODLD[data$MORTUCODLD==96] <- NA
  data$MORTNDI[data$MORTNDI==96] <- NA
  data$SMOKFREQNOW[data$SMOKFREQNOW==0|data$SMOKFREQNOW==7|data$SMOKFREQNOW==8|data$SMOKFREQNOW==9] <- NA

  return(data)
  
  }

