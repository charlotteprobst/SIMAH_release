model1_new <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt1_sophie.RDS")
model1_old <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt1.RDS")

summary(model1_new)

summarym1_old <- extractdata(model1_old) %>% mutate(model="old")
summarym1_new <- extractdata(model1_new) %>% mutate(model="new")

models <- rbind(summarym1_old, summarym1_new)
names <- names(models)[2:25]

rounding <- function(x){
  x <- round(x,digits=2)
  return(x)
}

models <- models %>% mutate_at(names, rounding) %>% select_if(~ !any(is.na(.))) %>% 
  pivot_longer(cols=`LEHS->SomeC1_Estimate`:`SomeC3->College_Upper`) %>% 
  separate(name, into=c("from","to","type")) %>% select_if(~ !any(is.na(.))) %>% 
  filter(Variable!="base") %>% mutate(Transition=paste(from, to, sep="_")) %>% 
  pivot_wider(names_from=type, values_from=value) %>% 
  mutate(EstimateCI = paste0(Estimate, "(",Lower,",",Upper,")")) %>% 
  dplyr::select(-c(Estimate,Lower,Upper,from,to)) %>% 
  pivot_wider(names_from=Transition, values_from=EstimateCI)
getwd()
write.csv(models, "model_comparison_education.csv", row.names=F)
