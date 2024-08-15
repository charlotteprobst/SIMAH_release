# set up latin hypercube for mortality parameters 

lhs <- sample_lhs(nsamples, PE)

for(i in 1:length(lhs)){
  lhs[[i]] <- data.frame(lhs[[i]])
  lhs[[i]]$sample <- i
}

lhs <- do.call(rbind,lhs)

# save samples - for wave 1 in Output Directory
write.csv(lhs, paste0(OutputDirectory, "/lhs_mortality-1",".csv"))

transitionsList <- list()
for(i in unique(lhs$sample)){
  transitionsList[[paste(i)]] <- lhs %>% filter(sample==i) %>% 
    ungroup() %>% 
    dplyr::select(-sample)
}

