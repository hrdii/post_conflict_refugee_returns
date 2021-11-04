##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-09 for Post-Conflict Refugee Returns Project
## Description: Adds Host state variables to conflict cases 
  ## Opportunity for employment
    ## % refugees at the end of conflict that are in host countries with high 
	## unemployment rate. Global average unemployment rate has varied between 
	## 4.4 and 6% over 1991-2019. A host country is arbitrarily defined as having
	## high unemployment if the rate exceeds 10%

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
library(stringr)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Load Conflict Cases
load("../Intermediate/Conflict case with refugee origin and host state information.RData")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")
## Load WB-UNHCR Country Name Lookup Dataframe
load("../Intermediate/WB-UNHCR Country Name Lookup.RData")
## Import Unemployment Rate Data
unemp_df <- fread("../Data/World Bank/Umemployment/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_672963.csv", 
                  skip = 4, header = TRUE) 

##### Clean Unemployment Rate Data


## Function to identify missing data
MissingIndex <- function(x, row = TRUE, n = 1){
  # x is a matrix or dataframe
  y <- is.na(x)
  m <- ifelse(row, 2, 1)
  n <- dim(x)[m] - n
  m <- ifelse(row, 1, 2)
  y <- apply(y, MARGIN = m, FUN = sum)
  y <- y >= n
  return(y)
}

## Drop completely empty rows
temp <- MissingIndex(unemp_df, row = TRUE, n = 4)
unemp_df <- unemp_df[!temp]

## Drop completely empty columns
temp <- MissingIndex(unemp_df, row = FALSE, n = 0)
unemp_df <- unemp_df[, .SD, .SDcols=-temp]

rm(temp)
## Drop other unnecessary columns
unemp_df <- unemp_df[, !c("Country Code", "Indicator Name", "Indicator Code")]

## Clean names
setnames(unemp_df, "Country Name", "country")

##### Harmonize Country Names

unemp_df$country <- NameCleaning(unemp_df$country)
## Replace Country names using Lookup Dataframe
unemp_df[lookup_df, on=.(country = wb_names), country := i.unhcr_names ]

rm(lookup_df)

##### Calculate % refugee

## Write a function to calculate % refugees
PropUnemp <- function(cas, r_df = refugee_df, econ_df = unemp_df, ref_threshold = 10){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to sources that host are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Subset Unemployment rate to year0
  ## we don't have unemployment data from before 1991
  if(y < 1991){
    y <- 1991:1993
    econ_df <- econ_df[, .SD, .SDcols= c("country", as.character(y))]
    econ_df <- econ_df[, unemp := rowSums(.SD, na.rm = TRUE)/3, .SDcols = as.character(y)]
    econ_df <- econ_df[, .SD, .SDcols = c("country", "unemp")]
  } else {
    econ_df <- econ_df[, .SD, .SDcols= c("country", as.character(y))]
    names(econ_df) <- c("country", "unemp") 
  }
  
  ## Add Unemployment rate for each host country
  hosts <- merge(x = hosts,
                 y = econ_df,
                 by = "country",
                 all.x = TRUE)
  ## Hosts are considered to have high unemployment if the rate exceeds ref_threshold
  hosts$high_unemp <- as.logical(
    as.character(
      cut(hosts$unemp, 
          breaks = c(-Inf, ref_threshold, Inf), 
          labels = c("FALSE", "TRUE"))))
  ## Calculate the proportion of source country refugees that live in host countries with high unemployment
  hosts$high_unemp <- hosts$high_unemp * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$high_unemp, na.rm = TRUE))
}

## Calculate % refugee
agg_df$punemp_5 <- sapply(agg_df$case, FUN = PropUnemp, ref_threshold = 5)
summary(agg_df$punemp_5)

agg_df$punemp_10 <- sapply(agg_df$case, FUN = PropUnemp, ref_threshold = 10)
summary(agg_df$punemp_10)

agg_df$punemp_15 <- sapply(agg_df$case, FUN = PropUnemp, ref_threshold = 15)
summary(agg_df$punemp_15)

## Save File
save(agg_df, 
     file = "../Intermediate/Conflict case with refugee origin and host state information.RData")

print("05e")
