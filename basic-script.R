########FINAL DATA PROJECT - DATA MANAGEMENT WITH R (Fall 2017)
####Kris Best

###STILL TO DO:
##Run actual regression
##Do regression visualisation
##Type up report in markdown
##Investigate shiny dashboard - if time



###Loading libraries 

library(tidyverse)
library(dplyr)
library(dbplyr)
#library(plyr)
#library(jsonlite)
library(DBI)
library(RSQLite)
library(knitr)
library(RColorBrewer)
library(wesanderson)
library(stargazer)
library(stringr)



###Downloading and extracting the data

unzip(zipfile="./history-scottish-witchcraft.zip",
      exdir=".")

datafolder <- "./history-scottish-witchcraft/data/"

file_list <- list.files(path=datafolder, pattern="*.csv")

for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(datafolder, file_list[i], sep=''), na.strings="")
  )}

#jsonData <- fromJSON("./history-scottish-witchcraft/datapackage.json")



###Culling variables I don't want, creating new variables

wdb_accused <- wdb_accused.csv %>% 
  select(accusedref,firstname,lastname,sex,age,ethnic_origin,maritalstatus,
         socioecstatus,occupation) %>% 
  mutate(female = ifelse(sex=="Female",TRUE,
                        ifelse(sex=="Male",FALSE,NA)))
#Fixing one irritating typo
wdb_accused[2326,"maritalstatus"]=NA

wdb_case <- wdb_case.csv %>% 
  select(caseref,accusedref,case_date,age_at_case,ends_with("_p")) %>% 
    #Creating logical values, combining some categories
    #Note: midwifery_p and unorthodoxrelpract_p had no "true" values, therefore dropped
    mutate(association = ifelse(consulting_p=="true"|implicatedbyanother_p=="true"|implicatedbyanother_p=="true",
                                  TRUE,FALSE),
           malevolentmagic = ifelse(demonic_p=="true"|demonic_possess_p=="true"|maleficium_p=="true",
                                      TRUE,FALSE),
           goodmagic = ifelse(fairies_p=="true"|folk_healing_p=="true"|whitemagic_p=="true",
                                TRUE,FALSE),
         political = ifelse(politicalmotive_p=="true"|treason_p=="true",TRUE,FALSE),
         property = ifelse(propertymotive_p=="true",TRUE,FALSE),
         neighbour = ifelse(neighbhd_dispute_p=="true",TRUE,FALSE),
         refusedcharity = ifelse(refusedcharity_p=="true",TRUE,FALSE),
         other = ifelse(other_p=="true",TRUE,FALSE),
         notenoughinfo = ifelse(notenoughinfo_p=="true",TRUE,FALSE)) %>% 
  select(-ends_with("_p"))

wdb_trial <- wdb_trial.csv %>% 
  select(trialref,caseref,female_accusers,male_accusers,high_status,verdict,
         sentence,execution,executionmethod) %>% 
  mutate(high_status = ifelse(high_status=="true",TRUE,FALSE),
         execution = ifelse(execution=="true",TRUE,FALSE),
         guilty = ifelse(verdict=="Guilty",TRUE,FALSE))


###Creating database

#Note: I am not creating a database from all available datasets. Only the ones that I
#considered to be useful for my specific topic.

con<-DBI::dbConnect(RSQLite::SQLite(),path = ":memory:")
dbWriteTable(con, "Accused", wdb_accused)
dbWriteTable(con, "Case", wdb_case)
dbWriteTable(con, "Trial", wdb_trial)
dbListTables(con)



###Creating local tibbles

accused_db <- tbl(con, "Accused")
accused <- accused_db %>% collect()

case_db <- tbl(con, "Case")
case <- case_db %>% collect()

trial_db <- tbl(con, "Trial")
trial <- trial_db %>% collect()

casetrial_db <- dplyr::left_join(case_db, trial_db, by="caseref")
fulldat_db <- dplyr::left_join(accused_db, casetrial_db, by="accusedref")
fulldat <- fulldat_db %>% collect()

dbDisconnect(con)



###Descriptive statistics - Demographics

summary(accused[, c('sex_l', 'age')])

#Sex
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

#Marital status
pal6 <- wes_palette(6, name = "GrandBudapest2", type = "continuous")
maritalvis <- ggplot(data = na.omit(subset(accused, select = c(maritalstatus))),
                 mapping = aes((x = maritalstatus), fill=factor(maritalstatus)))
maritalvis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=pal6, guide=FALSE) +
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.35)) +
  labs(title="Distribution of the accused by marital status",
       x="Marital status",
       y="Number of accused")

#Socioeconomic status
pal7 <- wes_palette(7, name = "GrandBudapest2", type = "continuous")
sociovis <- ggplot(data = na.omit(subset(accused, select = c(socioecstatus))),
                     mapping = aes((x = socioecstatus), fill=factor(socioecstatus)))
sociovis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=pal7, guide=FALSE) +
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.7)) +
  labs(title="Distribution of the accused by socioeconomic status",
       x="Socioeconomic status",
       y="Number of accused")


###Descriptive statistics - Accusation motives

summary(case[, c('association', 'malevolentmagic', 'goodmagic', 'political',
                 'property', 'neighbour', 'refusedcharity', 'other', 'notenoughinfo')])

association_count <- sum(case$association==TRUE)
malevolent_count <- sum(case$malevolentmagic==TRUE)
goodmagic_count <- sum(case$goodmagic==TRUE)
political_count <- sum(case$political==TRUE)
property_count <- sum(case$property==TRUE)
neighbour_count <- sum(case$neighbour==TRUE)
refusedcharity_count <- sum(case$refusedcharity==TRUE)
other_count <- sum(case$other==TRUE)
notenoughinfo_count <- sum(case$notenoughinfo==TRUE)

motivecounts <- c(association_count,malevolent_count,goodmagic_count,political_count,
                  property_count,neighbour_count,refusedcharity_count,other_count)
motives <- c("Association","Evil Magic","Good Magic","Political","Property",
                  "Neighbhd Dispute", "Refused Charity","Other")
motive.df <- data.frame(motives,motivecounts)

pal8 <- wes_palette(8, name = "GrandBudapest2", type = "continuous")
motivevis <- ggplot(data = motive.df,
                   mapping = aes(x = motives, y = motivecounts, fill=factor(motives)))
motivevis + geom_bar(stat="identity") + theme_classic() + 
  scale_fill_manual(values=pal8, guide=FALSE) + 
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.15),
        axis.text.x = element_text(angle = 45,hjust=1)) +
  labs(title="Count of motives for accusation",
       x="Motives",
       y="Number of cases")


###Descriptive statistics - Trial outcomes

summary(fulldat[, c('high_status', 'execution', 'verdict', 'executionmethod')])
verdictsum <- sum(!is.na(fulldat$verdict))
verdictsum #299 cases where a verdict was given

#Verdict
verdictvis <- ggplot(data = na.omit(subset(trial, select = c(verdict))),
                   mapping = aes((x = verdict), fill=factor(verdict)))
verdictvis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"), guide=FALSE) +
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.1)) +
  labs(title="Verdict (Where Known)",
       x="Verdict",
       y="Number of cases")

#Execution method (if executed and if known)
execmethod <- ggplot(data = na.omit(subset(trial, select = c(executionmethod))),
                     mapping = aes((x = executionmethod), fill=factor(executionmethod)))
execmethod + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"), guide=FALSE) +
  theme(axis.ticks = element_blank(), plot.title = element_text(hjust = -0.1)) +
  labs(title="Execution method (Where Known)",
       x="Execution method",
       y="Number of cases")



###Regression analysis

#Generating Guilty dependent variable

#Running model 1 - Guilty

m1 <- glm(guilty ~ sex + age + maritalstatus + socioecstatus + high_status + 
               malevolentmagic + goodmagic + political + property + neighbour + 
               refusedcharity + other, family=binomial(link='logit'), data=fulldat)
summary(m1)

m2 <- glm(guilty ~ sex + age + maritalstatus + socioecstatus + high_status, 
          family=binomial(link='logit'), data=fulldat)
summary(m2)

m3 <- lm(guilty ~ sex + age + maritalstatus + socioecstatus + high_status + 
            malevolentmagic + goodmagic + political + property + neighbour + 
            refusedcharity + other, data=fulldat)
summary(m3)