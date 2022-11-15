#' Load hepatitis B and C incidence data - to allocate new cases of hepatitis in each year
#'
#' @param
#' @keywords load hepatitis
#' @export
#' @examples
#' load_hepatitis_data
load_hepatitis_data <- function(SelectedState, proportion){

# read in the HBV / HCV data 
HepB <- read_csv("SIMAH_workplace/microsim/1_input_data/Incidence_HepB.csv") %>% 
  mutate(location_name = ifelse(location_name=="United States of America", "USA",
                                location_name)) %>% 
  filter(location_name==SelectedState) %>% 
  gather(agecat, Incidence, `15 to 19`:`80 plus`) %>% mutate(HepB = Incidence*proportion) %>% 
  mutate(microsim.init.sex = recode(sex_name, "Male" = "m", "Female" ="f")) %>% 
  mutate(agecat = gsub(" to ", "-", agecat)) %>% 
  mutate(microsim.init.sex = as.factor(microsim.init.sex)) %>% 
  dplyr::select(microsim.init.sex, year, agecat, HepB)

HepC <- read_csv("SIMAH_workplace/microsim/1_input_data/Incidence_HepC.csv") %>% 
  mutate(location_name = ifelse(location_name=="United States of America", "USA",
                                location_name)) %>% 
  filter(location_name==SelectedState) %>% 
    gather(agecat, Incidence, `15 to 19`:`80 plus`) %>% mutate(HepC=Incidence*proportion) %>%
    mutate(microsim.init.sex=recode(sex_name, "Male"="m", "Female"="f")) %>% 
    mutate(agecat = gsub(" to ", "-", agecat)) %>% 
    mutate(microsim.init.sex=as.factor(microsim.init.sex)) %>% 
    dplyr::select(microsim.init.sex, year, agecat, HepC)
  
  Hep <- left_join(HepB, HepC) %>% filter(year>=2000)
  rm(HepB, HepC)
  
  # read in the data on distributions of drinkers 
  distribution <- read.csv("SIMAH_workplace/microsim/1_input_data/drinkers_distribution.csv")
  names(distribution) <- gsub("X","",names(distribution))
  names(distribution) <- gsub("\\.","",names(distribution))
  
  # average drinking in population with HBV / HCV 41 g/day so subset the 40g/day distributions 
  distribution <- distribution %>% dplyr::select(Distributionamongdrinkersin, Sex, `40gday`) %>% 
    mutate(`40gday` = str_trim(`40gday`, side=c("right")),
           `40gday`= as.numeric(`40gday`),
           percentage = `40gday` / 100) %>% rename(microsim.init.sex=Sex,
                                                   gpdcat = Distributionamongdrinkersin) %>% 
    dplyr::select(microsim.init.sex, gpdcat, percentage) %>% 
    mutate(gpdcat = gsub(" g/day","",gpdcat),
           gpdcat = gsub("g/day","",gpdcat),
           gpdcat = gsub("[[:space:]]","",gpdcat),
           gpdcat = str_trim(gpdcat, side=c("right")))
  
  
  return(list(Hep,distribution))
}
