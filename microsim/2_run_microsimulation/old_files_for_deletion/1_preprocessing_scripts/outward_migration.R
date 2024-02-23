# processing outward migration data 
if(SelectedState!="USA"){
  outwardmigrants <- read.csv("SIMAH_workplace/microsim/1_input_data/outwardmigration_states.csv")

outwardmigrants <- outwardmigrants %>% mutate(NetMigration = abs(NET),
                                                Sex = recode(SEX, "M"="m","F"="f"),
                                                Age=AGE,
                                                Race=RACE,
                                                State=STATE,
                                                Year=YEAR) %>% 
  filter(State==SelectedState) %>% group_by(Year,Age,Sex,Race,State) %>% 
  summarise(NetMigration=sum(NetMigration)) %>% 
  mutate(Year=as.integer(Year),
         Age=as.integer(Age),
         Sex = as.character(Sex),
         Race= as.character(Race),
         NetMigration = round(NetMigration*proportion))
outwardmigrants <- data.frame(outwardmigrants)
}else if(SelectedState=="USA"){
  Year <- unique(inwardmigrants$Year)
  Race <- unique(inwardmigrants$Race)
  Age <- unique(inwardmigrants$Age)
  Sex <- unique(inwardmigrants$Sex)
  State <- "USA"
  outwardmigrants <- expand.grid(Year, Age, Sex, Race, State)
  names(outwardmigrants) <- c("Year","Age","Sex","Race","State")
  outwardmigrants$NetMigration <- 0
}
