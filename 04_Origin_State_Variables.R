##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-01-31 for Post-Conflict Refugee Returns Project
## Description: Adds Origin state variables to conflict cases 

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Load Data for all conflict cases
conflict_df <- fread(file = "../Intermediate/Cases with Active Conflict.csv")
## Load Conflict case with refugee information
load("../Intermediate/Conflict case with refugee information.RData")
## Load Historical GNI Data
inc_df <- fread("../Data/World Bank/Income/GNI per capita.csv")
## Load WB-UNHCR Country Name Lookup Dataframe
load("../Intermediate/WB-UNHCR Country Name Lookup.RData")

##### Security  
  ## Calculate number of years between Year1-10 when annual death toll was more than threshold

## Create a function to calculate the number of years with more then threshold casualties
CasualtyYears <- function(cas, df = conflict_df, ref_threshold = 25){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Years 1-10
  range <- c({y+1}:{y+10})
  ## Subset Causality df for range of years with death toll above threshold
  df <- df[country == s & year %in% range & deaths_total >= ref_threshold]
  return(nrow(df))
}

agg_df$cas25_year <- sapply(agg_df$case, FUN = CasualtyYears, ref_threshold = 25)
agg_df$cas500_year <- sapply(agg_df$case, FUN = CasualtyYears, ref_threshold = 500)
agg_df$cas1k_year <- sapply(agg_df$case, FUN = CasualtyYears, ref_threshold = 1000)

## Subset to conflict cases where the violence never goes up to 1k/year
# temp <- agg_df[agg_df$cas1k_year == 0, ]
# summary(temp$pc1_10_rr_year10)

##### Length of war  
agg_df$length_war <- agg_df$year0 - agg_df$start_year
#summary(agg_df$length_war)

##### GNI Per Capita

## Drop completely empty rows
inc_df <- inc_df[rowSums(is.na(inc_df)) < {ncol(inc_df) - 4}]
## Drop unnecessary columns
inc_df <- inc_df[, !c("Country Code", "Series Name", "Series Code")]
## Clean column names
setnames(inc_df, "Country Name", "country")
names(inc_df) <- gsub(" \\[YR[0-9]{4}\\]", "", names(inc_df))
## Drop Data before 1989
inc_df <- inc_df[, !as.character(1980:1988)]
## Harmonize Country Names
inc_df$country <- NameCleaning(inc_df$country)
## Replace Country names using Lookup Dataframe
inc_df[lookup_df, on=.(country = wb_names), country := i.unhcr_names ]

rm(lookup_df)

## Reshape income data from wide to long

inc_df <- melt(inc_df, id.vars = c("country"),
     measure.vars = as.character(1989:2018))
names(inc_df) <- c("country", "year", "gnipercap")
inc_df$year <- as.numeric(as.character(inc_df$year))

AddGNI <- function(cas, df = inc_df, change = TRUE){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Years 1-10
  range <- c(y:{y+10})
  ## Subset GNI data to range of years
  df <-df[country == s & year %in% range]
  ## Subset to not missing values
  df <- df[!is.na(gnipercap)]
  if(nrow(df) > 0){
    if(change){
      ## Calculate Average Annual % Change in GNI Per Capita
      yr_min <- min(df$year)
      yr_max <- max(df$year)
      gni_min <- df$gnipercap[df$year == yr_min]
      gni_max <- df$gnipercap[df$year == yr_max]
      temp <- {gni_max - gni_min} / gni_min / nrow(df)
    } else{
      ## Calculate Average GNI Per Capita
      temp <- mean(df$gnipercap)
    }
    return(temp)
  } else {
    return(NA)
  }
}

agg_df$avg_gnipercap <- sapply(agg_df$case, FUN = AddGNI, change = FALSE)
summary(agg_df$avg_gnipercap)

agg_df$avg_annual_gni_change <- sapply(agg_df$case, FUN = AddGNI, change = TRUE)
summary(agg_df$avg_annual_gni_change)

## Save File
save(agg_df, 
     file = "../Intermediate/Conflict case with refugee and origin state information.RData")

print("4")  
