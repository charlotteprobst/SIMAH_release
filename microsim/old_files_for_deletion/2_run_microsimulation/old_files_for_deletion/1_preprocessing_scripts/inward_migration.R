# processing inward migration data

inwardmigrants <- read.csv("SIMAH_workplace/microsim/1_input_data/inward_migration.csv")

inwardmigrants <- inwardmigrants %>% mutate(NetMigration = abs(Count)) %>% 
  filter(State==SelectedState) %>% group_by(Year,Age,Sex,Race,State) %>% 
  summarise(NetMigration=sum(NetMigration)) %>% 
  mutate(Year=as.integer(Year),
         Age=as.integer(Age),
         Sex = as.character(Sex),
         Race= as.character(Race),
         NetMigration = round(NetMigration*proportion))
