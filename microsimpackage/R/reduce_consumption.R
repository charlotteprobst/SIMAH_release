#' Reduce alcohol consumption for a given percentage reduction (to the whole population)
#'
#' @param
#' @keywords reduce alcohol
#' @export
#' @examples
#' reduce_consumption
reduce_consumption <- function(basepop, percentreduction){

# note this is just a demonstration example - real policies will be more complex
basepop$microsim.init.alc.gpd <- basepop$microsim.init.alc.gpd - (basepop$microsim.init.alc.gpd*percentreduction)

return(basepop)
}

