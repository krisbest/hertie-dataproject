########FINAL DATA PROJECT - DATA MANAGEMENT WITH R (Fall 2017)
####Kris Best

###STILL TO DO:
##MERGE DATASETS
##Check out / in marital status
##Clean datasets of unwanted columns
##Create categorical variables
##Run actual regression
##Do regression visualisation
##Type up report in markdown
##Investigate shiny dashboard - if time

###RELEVANT descriptive stats:
##Sex
##Age
##Ethnic status?
##Marital status
##Socioeconomic status
##Categoricals for accusation


###Loading libraries 

#library(tidyverse)
#library(dplyr)
#library(dbplyr)
#library(plyr)
#library(jsonlite)
#library(DBI)
#library(RSQLite)
#library(knitr)
#library(RColorBrewer)
#library(wesanderson)


###Downloading and extracting the data

#unzip(zipfile="./history-scottish-witchcraft.zip",
      #exdir=".")

datafolder <- "./history-scottish-witchcraft/data/"

file_list <- list.files(path=datafolder, pattern="*.csv")

for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(datafolder, file_list[i], sep=''), na.strings="")
  )}

#jsonData <- fromJSON("./history-scottish-witchcraft/datapackage.json")


###Creating database

#Note: I am not creating a database from all available datasets. Only the ones that I
#considered to be useful for my specific topic.

con<-DBI::dbConnect(RSQLite::SQLite(),path = ":memory:")
dbWriteTable(con, "Accused", wdb_accused.csv)
dbWriteTable(con, "Case", wdb_case.csv)
dbWriteTable(con, "Trial", wdb_trial.csv)
dbListTables(con)

###Dplyr manipulation

accused_db <- tbl(con, "Accused")
accused <- accused_db %>% collect()

case_db <- tbl(con, "Case")
case <- case_db %>% collect()

trial_db <- tbl(con, "Trial")
trial <- trial_db %>% collect()

###Descriptive statistics - visualisations

#Gender
gendervis <- ggplot(data = na.omit(subset(accused, select = c(sex))),
            mapping = aes((x = sex), fill=factor(sex)))
gendervis + geom_bar() + theme_classic() + 
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.25)) +
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest2"), guide=FALSE) +
  labs(title="Distribution of the accused by sex",
      x="Sex",
      y="Number of accused")

#Age
pal30 <- wes_palette(30, name = "GrandBudapest2", type = "continuous")
agevis <- ggplot(data = accused,
                mapping = aes((x = age)))
agevis + geom_histogram(bins=30,fill=pal30) + theme_classic() + 
  scale_fill_manual(values=pal30) +
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.15)) +
  labs(title="Distribution of the accused by age",
       x="Age",
       y="Number of accused")

#Marital#########FIX BY CHECKING /
pal6 <- wes_palette(6, name = "GrandBudapest2", type = "continuous")
maritalvis <- ggplot(data = na.omit(subset(accused, select = c(maritalstatus))),
                 mapping = aes((x = maritalstatus), fill=factor(maritalstatus)))
maritalvis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=pal6, guide=FALSE) +
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.35)) +
  labs(title="Distribution of the accused by marital status",
       x="Marital status",
       y="Number of accused")

#Socioeconomic
pal7 <- wes_palette(7, name = "GrandBudapest2", type = "continuous")
maritalvis <- ggplot(data = na.omit(subset(accused, select = c(socioecstatus))),
                     mapping = aes((x = socioecstatus), fill=factor(socioecstatus)))
maritalvis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=pal7, guide=FALSE) +
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.7)) +
  labs(title="Distribution of the accused by socioeconomic status",
       x="Marital status",
       y="Number of accused")


#dbDisconnect(con)