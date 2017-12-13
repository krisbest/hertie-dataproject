########FINAL DATA PROJECT - DATA MANAGEMENT WITH R (Fall 2017)
####Kris Best


###Loading libraries 

library(tidyverse)
library(dplyr)
library(dbplyr)
#library(plyr)
#library(jsonlite)
library(DBI)
library(RSQLite)


###Downloading and extracting the data

unzip(zipfile="./history-scottish-witchcraft.zip",
      exdir=".")

datafolder <- "./history-scottish-witchcraft/data/"

file_list <- list.files(path=datafolder, pattern="*.csv")

for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(datafolder, file_list[i], sep=''))
  )}

#jsonData <- fromJSON("./history-scottish-witchcraft/datapackage.json")


###Creating database

#Note: I'm aware that I could have looped this, but I wanted to clean up the names.

con<-DBI::dbConnect(RSQLite::SQLite(),path = ":memory:")
dbWriteTable(con, "Accused", wdb_accused.csv)
dbWriteTable(con, "Case", wdb_case.csv)
dbWriteTable(con, "CalendarCustom", wdb_calendarcustom.csv)
dbWriteTable(con, "CounterStrategy", wdb_counterstrategy.csv)
dbWriteTable(con, "DemonicPact", wdb_demonicpact.csv)
dbWriteTable(con, "DevilAppear", wdb_devilappearance.csv)
dbWriteTable(con, "ElfFairy", wdb_elf_fairyelements.csv)
dbWriteTable(con, "Malice", wdb_malice.csv)
dbWriteTable(con, "MusicalInst", wdb_musicalinstrument.csv)
dbWriteTable(con, "OtherCharges", wdb_othercharges.csv)
dbWriteTable(con, "Person", wdb_person.csv)
dbWriteTable(con, "PropertyDamage", wdb_propertydamage.csv)
dbWriteTable(con, "ReligiousMotif", wdb_religiousmotif.csv)
dbWriteTable(con, "RitualObject", wdb_ritualobject.csv)
dbWriteTable(con, "ShapeChange", wdb_shapechanging.csv)
dbWriteTable(con, "WeatherMod", wdb_weathermodification.csv)
dbWriteTable(con, "WhiteMagic", wdb_whitemagic.csv)
dbWriteTable(con, "WitchMeeting", wdb_witchesmeetingplace.csv)
dbListTables(con)


###Dplyr manipulation

accused_db <- tbl(con, "Accused")
accused <- accused_db %>% collect()

genderchart <- ggplot(data = accused,
            mapping = aes(x = sex))
genderchart + geom_bar()

