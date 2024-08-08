migration_cat <- function(data){
  data <- data %>% 
    mutate(inwardmigrant = ifelse(prevplace!="California" & currentplace=="California","CaliforniaIN",
                                  ifelse(prevplace!="Colorado" & currentplace=="Colorado","ColoradoIN",
                                         ifelse(prevplace!="Florida" & currentplace=="Florida","FloridaIN",
                                                ifelse(prevplace!="Indiana" & currentplace=="Indiana","IndianaIN",
                                                       ifelse(prevplace!="Kentucky" & currentplace=="Kentucky","KentuckyIN",
                                                              ifelse(prevplace!="Louisiana" & currentplace=="Louisiana","LouisianaIN",
                                                                     ifelse(prevplace!="Massachusetts" & currentplace=="Massachusetts","MassachusettsIN",
                                                                            ifelse(prevplace!="Michigan" & currentplace=="Michigan","MichiganIN",
                                                                                   ifelse(prevplace!="Minnesota" & currentplace=="Minnesota","MinnesotaIN",
                                                                                          ifelse(prevplace!="Missouri" & currentplace=="Missouri","MissouriIN",
                                                                                                 ifelse(prevplace!="New York" & currentplace=="New York","New YorkIN",
                                                                                                        ifelse(prevplace!="Oregon" & currentplace=="Oregon","OregonIN",
                                                                                                               ifelse(prevplace!="Pennsylvania" & currentplace=="Pennsylvania","PennsylvaniaIN",
                                                                                                                      ifelse(prevplace!="Tennessee" & currentplace=="Tennessee", "TennesseeIN",
                                                                                                                             ifelse(prevplace!="Texas" & currentplace=="Texas", "TexasIN", NA
                                                                                                                                    ))))))))))))))),
           outwardmigrant = ifelse(prevplace=="California" & currentplace!="California","CaliforniaOUT",
                                   ifelse(prevplace=="Colorado" & currentplace!="Colorado","ColoradoOUT",
                                          ifelse(prevplace=="Florida" & currentplace!="Florida","FloridaOUT",
                                                 ifelse(prevplace=="Indiana" & currentplace!="Indiana","IndianaOUT",
                                                        ifelse(prevplace=="Kentucky" & currentplace!="Kentucky","KentuckyOUT",
                                                               ifelse(prevplace=="Louisiana" & currentplace!="Louisiana","LouisianaOUT",
                                                                      ifelse(prevplace=="Massachusetts" & currentplace!="Massachusetts","MassachusettsOUT",
                                                                             ifelse(prevplace=="Michigan" & currentplace!="Michigan","MichiganOUT",
                                                                                    ifelse(prevplace=="Minnesota" & currentplace!="Minnesota","MinnesotaOUT",
                                                                                           ifelse(prevplace=="Missouri" & currentplace!="Missouri","MissouriOUT",
                                                                                                  ifelse(prevplace=="New York" & currentplace!="New York","New YorkOUT",
                                                                                                         ifelse(prevplace=="Oregon" & currentplace!="Oregon","OregonOUT",
                                                                                                                ifelse(prevplace=="Pennsylvania" & currentplace!="Pennsylvania","PennsylvaniaOUT",
                                                                                                                       ifelse(prevplace=="Tennessee" & currentplace!="Tennessee", "TennesseeOUT",
                                                                                                                              ifelse(prevplace=="Texas" & currentplace!="Texas", "TexasOUT", NA
                                                                                                                              ))))))))))))))))
  
  
  return(data)
  
  
  
  
}
