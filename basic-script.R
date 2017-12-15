########FINAL DATA PROJECT - DATA MANAGEMENT WITH R (Fall 2017)
####Kris Best

###STILL TO DO:
##Do regression visualisation
##Type up report in markdown
##Investigate shiny dashboard - if time



###Loading libraries 

#library(tidyverse)
#library(dplyr)
#library(dbplyr)
#library(DBI)
#library(RSQLite)
#library(knitr)
#library(RColorBrewer)
#library(wesanderson)
#library(stargazer)
#library(stringr)




###Downloading and extracting the data

#unzip(zipfile="./history-scottish-witchcraft.zip",
      #exdir=".")

datafolder <- "./history-scottish-witchcraft/data/"

file_list <- list.files(path=datafolder, pattern="*.csv")

for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(datafolder, file_list[i], sep=''), na.strings="")
  )}




###Culling variables I don't want, creating new variables

wdb_accused <- wdb_accused.csv %>% 
  select(accusedref,firstname,lastname,sex,age,ethnic_origin,maritalstatus,
         socioecstatus,occupation) %>% 
  mutate(female = ifelse(sex=="Female",TRUE,
                        ifelse(sex=="Male",FALSE,NA)),
         maritalstatus = ifelse(maritalstatus=="Married","Married",
                                ifelse(maritalstatus=="Single","Single",
                                       ifelse(maritalstatus=="Widowed","Widowed",NA))),
         socioecstatus = ifelse(socioecstatus=="Lower"|socioecstatus=="Landless"|socioecstatus=="Very Poor","Lower",
                                ifelse(socioecstatus=="Middling","Middling",
                                       ifelse(socioecstatus=="Upper"|socioecstatus=="Nobility/Chiefs"|socioecstatus=="Lairds/Baron","Upper",NA))))

wdb_case <- wdb_case.csv %>% 
  select(caseref,accusedref,case_date,age_at_case,ends_with("_p")) %>% 
    #Creating logical values, combining some categories
    #Note: midwifery_p and unorthodoxrelpract_p had no "true" values, therefore dropped
    mutate(association = ifelse(consulting_p=="true"|implicatedbyanother_p=="true"|implicatedbyanother_p=="true",
                                  TRUE,FALSE),
           evilmagic = ifelse(demonic_p=="true"|demonic_possess_p=="true"|maleficium_p=="true",
                                      TRUE,FALSE),
           goodmagic = ifelse(fairies_p=="true"|folk_healing_p=="true"|whitemagic_p=="true",
                                TRUE,FALSE),
                #Note: below are older categories I used before replacing all of them
                #with the category "non-magic"
                political = ifelse(politicalmotive_p=="true"|treason_p=="true",TRUE,FALSE),
                property = ifelse(propertymotive_p=="true",TRUE,FALSE),
                neighbour = ifelse(neighbhd_dispute_p=="true",TRUE,FALSE),
                refusedcharity = ifelse(refusedcharity_p=="true",TRUE,FALSE),
           nonmagic = ifelse(political==TRUE|property==TRUE|neighbour==TRUE|refusedcharity==TRUE,TRUE,FALSE),
           other = ifelse(other_p=="true",TRUE,FALSE),
           notenoughinfo = ifelse(notenoughinfo_p=="true",TRUE,FALSE)) %>% 
  select(-ends_with("_p"))

wdb_trial <- wdb_trial.csv %>% 
  select(trialref,caseref,female_accusers,male_accusers,high_status,verdict,
         sentence,execution,executionmethod) %>% 
  mutate(high_status = ifelse(high_status=="true",TRUE,FALSE),
         execution = ifelse(execution=="true",TRUE,FALSE),
         guilty = ifelse(verdict=="Guilty",TRUE,FALSE),
         nr_accusers = female_accusers + male_accusers)





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

#Correcting column types that were changed during the database pull
fulldat$sex <- as.factor(fulldat$sex)
fulldat$ethnic_origin <- as.factor(fulldat$ethnic_origin)
fulldat$maritalstatus <- as.factor(fulldat$maritalstatus)
fulldat$socioecstatus <- as.factor(fulldat$socioecstatus)
fulldat$verdict <- as.factor(fulldat$verdict)
fulldat$executionmethod <- as.factor(fulldat$executionmethod)
fulldat$female <- as.logical(fulldat$female)
fulldat$association <- as.logical(fulldat$association)
fulldat$evilmagic <- as.logical(fulldat$evilmagic)
fulldat$goodmagic <- as.logical(fulldat$goodmagic)
fulldat$nonmagic <- as.logical(fulldat$nonmagic)
fulldat$political <- as.logical(fulldat$political)
fulldat$property <- as.logical(fulldat$property)
fulldat$neighbour <- as.logical(fulldat$neighbour)
fulldat$refusedcharity <- as.logical(fulldat$refusedcharity)
fulldat$other <- as.logical(fulldat$other)
fulldat$notenoughinfo <- as.logical(fulldat$notenoughinfo)
fulldat$high_status <- as.logical(fulldat$high_status)
fulldat$execution <- as.logical(fulldat$execution)
fulldat$guilty <- as.logical(fulldat$guilty)




###Descriptive statistics - Demographics

summary(accused[, c('female', 'age', 'maritalstatus', 'socioecstatus')])

#Observation counts
female_notNA <- sum(!is.na(accused$female))
age_notNA <- sum(!is.na(accused$age))
maritalstatus_notNA <- sum(!is.na(accused$maritalstatus))
socioecstatus_notNA <- sum(!is.na(accused$socioecstatus))
female_notNA 
age_notNA 
maritalstatus_notNA 
socioecstatus_notNA

#Graphic - Sex
gendervis <- ggplot(data = na.omit(subset(accused, select = c(sex))),
            mapping = aes((x = sex), fill=factor(sex)))
gendervis + geom_bar() + theme_classic() + 
  theme(axis.ticks = element_blank()) +
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest2"), guide=FALSE) +
  labs(title="Distribution of the accused by sex",
       subtitle="Where known, observations = 3170",
      x="",
      y="Number of accused")

#Graphic - Age
pal30 <- wes_palette(30, name = "GrandBudapest2", type = "continuous")
agevis <- ggplot(data = accused,
                mapping = aes((x = age)))
agevis + geom_histogram(bins=30,fill=pal30) + theme_classic() + 
  scale_fill_manual(values=pal30) +
  theme(axis.ticks = element_blank()) +
  labs(title="Distribution of the accused by age",
       subtitle="Where known, observations = 166",
       x="",
       y="Number of accused")

#Graphic - Marital status
maritalvis <- ggplot(data = na.omit(subset(accused, select = c(maritalstatus))),
                 mapping = aes((x = maritalstatus), fill=factor(maritalstatus)))
maritalvis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest2"), guide=FALSE) +
  theme(axis.ticks = element_blank()) +
  labs(title="Distribution of the accused by marital status",
       subtitle="Where known, observations = 736",
       x="",
       y="Number of accused")

#Graphic - Socioeconomic status
sociovis <- ggplot(data = na.omit(subset(accused, select = c(socioecstatus))),
                     mapping = aes((x = socioecstatus), fill=factor(socioecstatus)))
sociovis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=3, name="GrandBudapest2"), guide=FALSE) +
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle = 45,hjust=1)) +
  labs(title="Distribution of the accused by socioeconomic status",
       subtitle="Where known, observations = 362",
       x="",
       y="Number of accused")



###Descriptive statistics - Accusation motives

#Some of the accused were accused more than once, so the full data set is used here

summary(fulldat[, c('association', 'evilmagic', 'goodmagic', 'nonmagic',
                    'political', 'property', 'neighbour', 'refusedcharity', 'other', 'notenoughinfo')])

association_count <- sum(fulldat$association,na.rm=TRUE)
evilmagic_count <- sum(fulldat$evilmagic,na.rm=TRUE)
goodmagic_count <- sum(fulldat$goodmagic,na.rm=TRUE)
nonmagic_count <- sum(fulldat$nonmagic,na.rm=TRUE)
political_count <- sum(fulldat$political,na.rm=TRUE)
property_count <- sum(fulldat$property,na.rm=TRUE)
neighbour_count <- sum(fulldat$neighbour,na.rm=TRUE)
refusedcharity_count <- sum(fulldat$refusedcharity,na.rm=TRUE)
other_count <- sum(fulldat$other,na.rm=TRUE)
notenoughinfo_count <- sum(fulldat$notenoughinfo,na.rm=TRUE)

totalmotive_count <- sum(fulldat$association==TRUE|fulldat$evilmagic==TRUE|fulldat$goodmagic==TRUE|fulldat$nonmagic==TRUE|fulldat$other==TRUE,na.rm=TRUE)
totalmotive_count

#Counts with old motive classification
motivecountsold <- c(association_count,evilmagic_count,goodmagic_count,political_count,
                  property_count,neighbour_count,refusedcharity_count,other_count)
motivesold <- c("Association","Evil Magic","Good Magic","Political","Property",
                  "Neighbhd Dispute", "Refused Charity","Other")
motiveold.df <- data.frame(motivesold,motivecountsold)

#Counts with new motive classification
motivecounts <- c(association_count,evilmagic_count,goodmagic_count,nonmagic_count,
                  other_count)
motives <- c("Association","Evil Magic","Good Magic","Non-Magic","Other")
motive.df <- data.frame(motives,motivecounts)

pal5 <- wes_palette(5, name = "GrandBudapest2", type = "continuous")
motivevis <- ggplot(data = motive.df,
                   mapping = aes(x = motives, y = motivecounts, fill=factor(motives)))
motivevis + geom_bar(stat="identity") + theme_classic() + 
  scale_fill_manual(values=pal5, guide=FALSE) + 
  theme(axis.ticks = element_blank(), axis.text.x = element_text(angle = 45,hjust=1)) +
  labs(title="Count of motives for accusation",
       subtitle="Where known, observations = 409",
       x="",
       y="Number of cases")


###Descriptive statistics - Trial outcomes

summary(fulldat[, c('high_status', 'execution', 'verdict', 'executionmethod')])
verdictcount <- sum(!is.na(fulldat$verdict))
verdictcount #299 cases where a verdict was given
executionmethodcount <- sum(!is.na(fulldat$executionmethod))
executionmethodcount #175 cases where an execution method was noted
executecount <- sum(fulldat$execution==TRUE|fulldat$execution==FALSE,na.rm=TRUE)
executecount #3210 cases with data on whether or not there was an execution

#Verdict
verdictvis <- ggplot(data = na.omit(subset(trial, select = c(verdict))),
                   mapping = aes((x = verdict), fill=factor(verdict)))
verdictvis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"), guide=FALSE) +
  theme(axis.ticks = element_blank()) +
  labs(title="Verdict",
       subtitle="Where known, observations = 299",
       x="",
       y="Number of cases")

#Verdict - By Sex
verdict_noNA <- fulldat %>%
  filter(!is.na(verdict))
verdictvis2 <- ggplot(data = verdict_noNA,
                     mapping = aes((x = verdict), fill=factor(sex)))
verdictvis2 + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2")) +
  theme(axis.ticks = element_blank()) +
  labs(title="Verdict",
       subtitle="Where known, observations = 299",
       x="",
       y="Number of cases")

#Executed - by sex
executed_noNA <- fulldat %>%
  filter(!is.na(execution))
executevis <- ggplot(data = executed_noNA,
                        mapping = aes((x = execution), fill=factor(sex)))
executevis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2")) +
  theme(axis.ticks = element_blank()) +
  labs(title="Executed",
       subtitle="Where known, observations = 3210",
       x="",
       y="Number of cases")

#Execution method (if executed and if known)
execmethodvis <- ggplot(data = na.omit(subset(trial, select = c(executionmethod))),
                     mapping = aes((x = executionmethod), fill=factor(executionmethod)))
execmethodvis + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest2"), guide=FALSE) +
  theme(axis.ticks = element_blank()) +
  labs(title="Execution method",
       subtitle="Where known, observations = 175",
       x="",
       y="Number of cases")

#Execution method - by sex
execmethod_noNA <- fulldat %>%
  filter(!is.na(executionmethod))
execmethodvis2 <- ggplot(data = execmethod_noNA,
                        mapping = aes((x = executionmethod), fill=factor(sex)))
execmethodvis2 + geom_bar() + theme_classic() + 
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2")) +
  theme(axis.ticks = element_blank()) +
  labs(title="Execution method",
       subtitle="Where known, observations = 175",
       x="",
       y="Number of cases")



###Regression analysis

#Creating limited dataset
guilty_noNA <- fulldat %>% 
  filter(!is.na(guilty))

#Running model 1 - Found guilty, logit
m1 <- glm(guilty ~ female + maritalstatus + association +
          evilmagic + goodmagic + nonmagic, family=binomial(link='logit'),
          data=guilty_noNA)
summary(m1)
exp(coefficients(m1))

#Running model 2 - Found guilty, linear probability model
m2 <- lm(guilty ~ female + maritalstatus + association +
            evilmagic + goodmagic + nonmagic, data=guilty_noNA)
summary(m2)

#Running model 3 - Executed, logit
m3 <- glm(execution ~ female + maritalstatus + association +
            evilmagic + goodmagic + nonmagic, family=binomial(link='logit'),
          data=executed_noNA)
summary(m3)
exp(coefficients(m3))

#Running model 4 - Executed, linear probability model
m4 <- lm(guilty ~ female + maritalstatus + association +
           evilmagic + goodmagic + nonmagic, data=executed_noNA)
summary(m4)