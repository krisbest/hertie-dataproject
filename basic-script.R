########FINAL DATA PROJECT - DATA MANAGEMENT WITH R (Fall 2017)
####Kris Best


###Loading libraries 

library(tidyverse)
library(dplyr)
library(jsonlite)
library(plyr)


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


###