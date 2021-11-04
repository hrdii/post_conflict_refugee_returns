##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-01-30 for Post-Conflict Refugee Returns Project
## Description: Cleans UNHCR Refugee data  

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Loading Refugee data
refugee_df <- read.csv("../Data/UNHCR/Refugees (incl. refugee-like situations).csv", 
                        skip = 3, header = T, stringsAsFactors = F)

##### Cleaning Refugee Data

## Subsetting to refugees after 1989  
refugee_df <- setDT(refugee_df[refugee_df$Year >= 1989, 
                           c(1,2,3,5)])
names(refugee_df) <- c("year", "host", "source", "pop")

##### Harmozining Country Names

cols <- c("host", "source")
refugee_df <- refugee_df[, (cols) := lapply(.SD, FUN = NameCleaning), .SDcols = cols]
refugee_df <- refugee_df[, (cols) := lapply(.SD, FUN = gsub, pattern = "Viet Nam", replacement = "Vietnam"), .SDcols = cols]
refugee_df <- refugee_df[, (cols) := lapply(.SD, FUN = gsub, pattern = "Iran Islamic Republic", replacement = "Iran"), .SDcols = cols]
refugee_df <- refugee_df[, (cols) := lapply(.SD, FUN = gsub, pattern = "CÃ te d Ivoire", replacement = "Ivory Coast"), .SDcols = cols]

##### Reshaping and Aggregating Refugee Data

## Reshape
refugee_df <- reshape(refugee_df, 
                           v.names="pop", 
                           timevar="host", 
                           idvar=c("year", "source"),
                           direction="wide")
names(refugee_df) <- gsub("pop.", "", names(refugee_df))

cols <- names(refugee_df)[3:ncol(refugee_df)]
refugee_df <- refugee_df[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
refugee_df[is.na(refugee_df)] <- 0

## Aggregate per year per country
refugee_df <- refugee_df[, total := rowSums(.SD), .SDcol = cols]
refugee_df <- setcolorder(refugee_df, c("year", "source", "total", cols))

rm(cols)


host_countries <- names(refugee_df)
host_countries <- setdiff(host_countries, c("year", "source", "total"))

##### Create a list of major refugee source
ref_source <- refugee_df[total >= 10000, unique(source)]
ref_source <- setdiff(ref_source, c("Various Unknown", "Stateless"))

## Save File
save(refugee_df, file = "../Intermediate/Refugee Population.RData")

## Subsetting to refugees from major refugee sources
refugee_df <- refugee_df[source %in% ref_source]

## Save file
write.csv(refugee_df, "../Output/Refugee Population from shortlisted sources to all hosts 1989-2018.csv")
save(ref_source, file = "../Intermediate/Longlist of Major Refugee Sources.RData")

print("1a")