##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-10 for Post-Conflict Refugee Returns Project
## Description: Create a lookup dataframe to harmonize WB and UNHCR countries

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Load Historical GNI Data
load(file = "../Intermediate/GNI Per Capita.RData")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")

## Create a list of all Host Countries
host_countries <- names(refugee_df)
host_countries <- setdiff(host_countries, c("year", "source", "total"))

rm(refugee_df)

## Create a list of all WB Countries
wb_names <- inc_df$country

rm(inc_df)

##### Create Lookup Dataframe

## All WB Countries not matched with host countries list
lookup_df <- data.table(wb_names = setdiff(wb_names, host_countries))

## Matches for countries in the world map from host countries list
lookup_df$unhcr_names <- NA
lookup_df$unhcr_names[lookup_df$wb_names == "Bolivia"] <- "Bolivia Plurinational State"
lookup_df$unhcr_names[lookup_df$wb_names == "Congo Democratic Republic"] <- "Democratic Republic Congo"
lookup_df$unhcr_names[lookup_df$wb_names == "Congo Republic"] <- "Congo"
lookup_df$unhcr_names[grep("Ivoire", lookup_df$wb_names)] <- "Ivory Coast"
lookup_df$unhcr_names[lookup_df$wb_names == "Dominica"] <- "Dominican Republic"
lookup_df$unhcr_names[lookup_df$wb_names == "Egypt Arab Republic"] <- "Egypt"
lookup_df$unhcr_names[lookup_df$wb_names == "Hong Kong SAR China"] <- "China Hong Kong SAR"
lookup_df$unhcr_names[lookup_df$wb_names == "Iran Islamic Republic"] <- "Iran"
lookup_df$unhcr_names[lookup_df$wb_names == "Korea Republic"] <- "Republic Korea"
lookup_df$unhcr_names[lookup_df$wb_names == "Kyrgyz Republic"] <- "Kyrgyzstan"
lookup_df$unhcr_names[lookup_df$wb_names == "Lao PDR"] <- "Lao People s Democratic Republic"
lookup_df$unhcr_names[lookup_df$wb_names == "Macao SAR China"] <- "China Macao SAR"
lookup_df$unhcr_names[lookup_df$wb_names == "North Macedonia"] <- "former Yugoslav Republic Macedonia"
lookup_df$unhcr_names[lookup_df$wb_names == "Moldova"] <- "Republic Moldova"
lookup_df$unhcr_names[lookup_df$wb_names == "Serbia"] <- "Serbia Kosovo S RES"
lookup_df$unhcr_names[lookup_df$wb_names == "Slovak Republic"] <- "Slovakia"
lookup_df$unhcr_names[lookup_df$wb_names == "Tanzania"] <- "United Republic Tanzania"
lookup_df$unhcr_names[lookup_df$wb_names == "United States"] <- "United States America"
lookup_df$unhcr_names[grep("Venezuela", lookup_df$wb_names)] <- "Venezuela Bolivarian Republic"
lookup_df$unhcr_names[lookup_df$wb_names == "Yemen Republic"] <- "Yemen"
## Drop incomplete cases
lookup_df <- lookup_df[complete.cases(lookup_df)]

save(lookup_df, file = "../Intermediate/WB-UNHCR Country Name Lookup.RData")

print("1c")