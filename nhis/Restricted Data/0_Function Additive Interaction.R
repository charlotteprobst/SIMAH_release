############## ESTIMATES AND INFERENCE FOR ADDITIVE AND MECHANISTIC INTERACTIONS ############## 

##### Please cite as follows:
##### Mathur MB & VanderWeele TJ (2018). R function for additive interaction measures. Epidemiology 29(1), e5-e6.

# For more details, visit: https://osf.io/7ccpp/



##### Fn: Checks whether both exposure ORs are > 1
causative = function( OR10, OR01 ) {
  return( (OR10 > 1) & (OR01 > 1) )
}

##### Fn: Compute additive interaction measures with inference
additive_interactions = function( model, exposure1, exposure2, monotone=0, CI.level=0.95) {
  
  # variables to keep for delta method
  #in order to get the right variables in the interaction string
  interaction.string = paste(exposure1, exposure2, sep=":") 
  
  if ( !(interaction.string %in% names(coef(model) ) ) ) {
    stop("Could not identify the interaction coefficient in the model formula, possibly because you used the I() operator. Please enter the interaction using * instead.")
  }
  
  keepers = c(exposure1, exposure2, interaction.string)
  
  
  # get variance-covariance matrix of the model
  V = as.matrix( vcov(model) )
  V2 = V[keepers, keepers]  # subset to just the coefficients of interest
  
  # calculate various coefficients and ORs
  b10 = coef(model)[exposure1]
  b01 = coef(model)[exposure2]
  bint = coef(model)[interaction.string]
  OR00 = 1.0  # reference
  OR10 = exp(b10)
  OR01 = exp(b01)
  OR11 = exp( b10 + b01 + bint )
  
  # check if we are already in causative case
  if( causative( OR10 = OR10, OR01 = OR01 ) ) {
    
    # RERI (VanderWeele pg. 258-259)
    RERI = OR11 - exp(b10) - exp(b01) + 1
    
    require(msm)
    SE.RERI = deltamethod( ~ exp( x1 + x2 + x3 ) - exp(x1) - exp(x2) + 1,
                           mean=c( coef(model)[exposure1], coef(model)[exposure2], coef(model)[interaction.string] ), 
                           cov=V2)  # this is its SE
    
    # attributable proportion (VanderWeele pg. 256)
    AP = RERI / OR11
    SE.AP = deltamethod( ~ ( exp( x1 + x2 + x3 ) - exp(x1) - exp(x2) + 1 ) / exp( x1 + x2 + x3 ),
                         mean=c( coef(model)[exposure1], coef(model)[exposure2], coef(model)[interaction.string] ),
                         cov=V2)  # this is its SE
    
    alpha = 1-CI.level
    z = qnorm( 1 - alpha/2 )  # critical value
    
    # RERI p-values for tests vs. nulls of 0, 1, or 2
    # for mechanistic conclusions (VanderWeele pg. 275)
    p.0 = 1 - pnorm( RERI/SE.RERI )
    p.1 = 1 - pnorm( (RERI-1) / SE.RERI )
    p.2 = 1 - pnorm( (RERI-2) / SE.RERI )
    
    # based on monotonicity assumption, give p-value for epistatic and sufficient-cause interactions
    if (monotone==0) {
      p.val.epi = p.2
      p.val.suff.cause = p.1
    } else if (monotone==1) {
      p.val.epi = p.val.suff.cause = p.1
    } else if (monotone==2) {
      p.val.epi = p.val.suff.cause = p.0
    } else {
      stop("Argument 'monotone' must be 0, 1, or 2")
    }
    
    ##### Prop. of joint effect due to each exposure separately or to their interaction ####
    # VanderWeele pg. 282
    denom = OR11 - 1
    E1.contrib = ( exp( coef(model)[exposure1] ) - 1 ) / denom 
    E2.contrib = ( exp( coef(model)[exposure2] ) - 1 ) / denom 
    int.contrib = RERI / denom
    
    SE.1 = deltamethod( ~ ( exp(x1) - 1 ) / ( exp( x1 + x2 + x3 ) - 1 ),
                        mean=c( coef(model)[exposure1], coef(model)[exposure2], coef(model)[interaction.string] ),
                        cov=V2)
    SE.2 = deltamethod( ~ ( exp(x2) - 1 ) / ( exp( x1 + x2 + x3 ) - 1 ),
                        mean=c( coef(model)[exposure1], coef(model)[exposure2], coef(model)[interaction.string] ),
                        cov=V2)
    SE.3 = deltamethod( ~ ( exp( x1 + x2 + x3 ) - exp(x1) - exp(x2) + 1 ) / ( exp( x1 + x2 + x3 ) - 1 ),
                        mean=c( coef(model)[exposure1], coef(model)[exposure2], coef(model)[interaction.string] ),
                        cov=V2)
    
    alpha = 1-CI.level
    z = qnorm( 1 - alpha/2 )  # critical value
    
    ##### Return results #####
    rs = data.frame( Stat = c( "RERI", "AP", exposure1, exposure2, interaction.string ),
                     Est = c( RERI, AP, E1.contrib, E2.contrib, int.contrib ),
                     CI.lo = c( RERI - z*SE.RERI, AP - z*SE.AP, E1.contrib - z*SE.1, E2.contrib - z*SE.2, int.contrib - z*SE.3 ),
                     CI.hi = c( RERI + z*SE.RERI, AP + z*SE.AP, E1.contrib + z*SE.1, E2.contrib + z*SE.2, int.contrib + z*SE.3 ),
                     p.val.0 = c( p.0, ( 1 - pnorm( abs(AP)/SE.AP) ), ( 1 - pnorm(E1.contrib/SE.1) ), ( 1 - pnorm(E2.contrib/SE.2) ), ( 1 - pnorm(int.contrib/SE.3) ) ), 
                     p.val.epi = c( p.val.epi, NA, NA, NA, NA ), 
                     p.val.suff.cause = c( p.val.suff.cause, NA, NA, NA, NA )
    )
    rownames(rs) = NULL
    
  } else{
    
    ## return NAs if not in causative case: At least one exposure has negative association with outcome
    rs = data.frame( Stat = c( "RERI", "AP", exposure1, exposure2, interaction.string ),
                     Est = rep(NA, 5),
                     CI.lo = rep(NA, 5),
                     CI.hi = rep(NA, 5),
                     p.val.0 = rep(NA, 5), 
                     p.val.epi = rep(NA, 5), 
                     p.val.suff.cause = rep(NA, 5)
    )
    rownames(rs) = NULL
    
  } 
 
  if (monotone == 1) cat("\nAssuming AT LEAST ONE of exposures has positive monotonic effect\n\n")
  if (monotone == 2) cat("\nAssuming BOTH exposures have positive monotonic effect\n\n")
  #print(rs)
  invisible(rs)
}
