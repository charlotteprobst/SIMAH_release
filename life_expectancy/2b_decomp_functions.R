## Life expectancy decomposition
## Functions



#LIFETABLE
# Horiuchi decomposition: each age-specific rate is slightly perturbed -> see change in LE that produces
# function for estimating LE (massive vector of age-cause-specific rates)
life_table_causes <- function(nmx_by_cause_vector, age_vector, ax_vector) {
      
      #Make the blank life table
      life_table <- matrix(nrow = length(age_vector), ncol = 11)
      colnames(life_table) <- c("age","n","mx","ax","qx","px","lx","dx","Lx","Tx","ex")
      life_table[,1] <- age_vector
      life_table[,4] <- ax_vector
      
      #Figure out number of causes
      ncauses <- length(nmx_by_cause_vector) / length(age_vector)
      
      #n
      for(i in 1:(length(age_vector)-1)) {
            j = i+1
            life_table[i,2] <- life_table[j,1] - life_table[i,1]
      }
      
      #mx - calculate overall mx as the sum of cause-specific mx's
      life_table[,3] <- rowSums(matrix(nmx_by_cause_vector,ncol = ncauses))
      
      #qx
      life_table[,5] <- (life_table[,2]*life_table[,3]) / (1 + (life_table[,2]-life_table[,4])*life_table[,3])
      life_table[length(age_vector),5] <- 1
      
      #px
      life_table[,6] <- 1 - life_table[,5]
      
      #lx
      life_table[1,7] <- 100000
      for(i in 2:length(age_vector)) {
            
            j = i - 1
            
            life_table[i,7] <- life_table[j,7]*life_table[j,6]
      }
      
      #dx
      life_table[,8] <- life_table[,7]*life_table[,5]
      
      #Lx
      life_table[,9] <- (life_table[,2]*life_table[,7]*life_table[,6]) + (life_table[,4]*life_table[,8])
      life_table[length(age_vector),9] <- life_table[length(age_vector),8] / life_table[length(age_vector),3]
      
      #Tx
      life_table[length(age_vector),10] <- life_table[length(age_vector),9]
      for(i in (length(age_vector)-1):1) {
            
            j <- i + 1
            
            life_table[i,10] <- life_table[j,10] + life_table[i,9]
      }
      
      #ex
      life_table[,11] <- life_table[,10] / life_table[,7]
      
      #e0
      e0 <- life_table[1,11]
      
      #Output just LE at birth, could output the entire LT if you wanted
      return(e0)
      
}
