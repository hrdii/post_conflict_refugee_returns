##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-01-30 for Post-Conflict Refugee Returns Project
## Description: Cleans UNHCR Refugee Returned data  

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Loading Refugee Return data
rr_df <- read.csv("../Data/UNHCR/Returned refugees.csv", 
                               skip = 3, header = T, stringsAsFactors = F)
## Loading Shortlist of Major Refugee Sources
load("../Intermediate/Shortlist of Major Refugee Sources.RData")

##### Cleaning Refugee Data
## Subsetting to refugees after 1989  
rr_df <- setDT(rr_df[rr_df$Year >= 1989, 
                                 c(1,2,3,5)])
names(rr_df) <- c("year", "host", "source", "pop")

##### Harmozining Country Names

cols <- c("host", "source")
rr_df <- rr_df[ , (cols) := lapply(.SD, NameCleaning), .SDcols = cols]

lookup_df <- data.table(ref_names = setdiff(major_source, union(rr_df$host, rr_df$source)))
lookup_df$unhcr_names[lookup_df$ref_names == "Iran"] <- "Iran Islamic Republic"
lookup_df$unhcr_names[lookup_df$ref_names == "Ivory Coast"] <- "CÃ te d Ivoire"

rr_df[lookup_df, on=.(source = unhcr_names), source := i.ref_names ]
rr_df[lookup_df, on=.(host = unhcr_names), host := i.ref_names ]

rm(lookup_df)

##### Reshaping and Aggregating Refugee Data

## Reshape
rr_df <- reshape(rr_df, 
                           v.names="pop", 
                           timevar="host", 
                           idvar=c("year", "source"),
                           direction="wide")
names(rr_df) <- gsub("pop.", "", names(rr_df))

cols <- names(rr_df)[3:ncol(rr_df)]
rr_df <- rr_df[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
rr_df[is.na(rr_df)] <- 0

## Aggregate per year per country
rr_df <- rr_df[, total := rowSums(.SD), .SDcol = cols]
rr_df <- setcolorder(rr_df, c("year", "source", "total", cols))

rm(cols)

## Save File
save(rr_df, file = "../Intermediate/Refugee Return Population.RData")

## Subsetting to refugees from major refugee sources
rr_df <- rr_df[source %in% major_source]

## Save file
write.csv(rr_df, "../Output/Refugee Returned Population from shortlisted sources to all hosts 1989-2018.csv")

print("3b")
