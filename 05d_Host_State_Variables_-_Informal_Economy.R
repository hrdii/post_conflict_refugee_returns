##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-10 for Post-Conflict Refugee Returns Project
## Description: Adds Host state variables to conflict cases 
  ## Opportunity for employment
    ## % refugees at the end of conflict that are in host countries that have 
	## significant informal economies. The average proportion of informal 
	## economy in 35 high-income OECD countries is 20.21%. A host country is
	## defined as Informal if the average proportion of informal economy is 
	## greater the 20%

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
## Import Informal Economy Data
informal_df <- read.delim("../Data/Informal Economy.txt", stringsAsFactors = FALSE)

##### Clean Informal Economy Data

## Split by space (This will create more than one elements where country names have more than one words)
informal_df <- strsplit(informal_df[, 1], split = " ")
## Remove serial numbers
informal_df <- lapply(informal_df, function(x) x[-1])
## Write a Function to combine country names have more than one words
CombineNames <- function(x){
  n <- length(x) 
  if(n>17){
    x <- c(paste(x[1:{n-16}], collapse = " "), x[{n-15}:n])
  }
  return(x)
}
## Combine country names to create vectors of same length  
informal_df <- lapply(informal_df, function(x) CombineNames(x))
## Create Dataframe from list 
informal_df <- as.data.frame(do.call(rbind, informal_df), stringsAsFactors = F)
## Drop yearly data and retain only the average column
informal_df <- informal_df[, c(1, ncol(informal_df))]
## Rename columns 
names(informal_df) <- c("country", "average")
## Clean data type
informal_df$average <- as.numeric(informal_df$average)
## Conver to data.table
informal_df <- setDT(informal_df)

##### Harmonize Country Names

informal_df$country <- NameCleaning(informal_df$country)

## Create a list of all countries
all_names <- informal_df$country

## All countries not matched with host countries list
host_countries <- names(refugee_df)
host_countries <- setdiff(host_countries, c("year", "source", "total"))

temp <- setdiff(all_names, host_countries)
temp <- setdiff(temp, lookup_df$wb_names)

lookup_df$wb_names[grep("Ivoire", lookup_df$wb_names)] <- "Cote d Ivoire"
lookup_df$wb_names[grep("Macedonia", lookup_df$wb_names)] <- "Macedonia FYR"
lookup_df$wb_names[grep("Venezuela", lookup_df$wb_names)] <- "Venezuela RB"

## Replace Country names using Lookup Dataframe
informal_df[lookup_df, on=.(country = wb_names), country := i.unhcr_names ]

rm(lookup_df)
rm(temp)

##### Calculate % refugee

## Write a function to calculate % refugees
PropInformal <- function(cas, r_df = refugee_df, econ_df = informal_df, ref_threshold = 20){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to host that are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Add Informal Economy size for each host country
  hosts <- merge(x = hosts,
                 y = econ_df,
                 by = "country",
                 all.x = TRUE)
  ## Hosts are considered to have large informal economy if the average size exceeds ref_threshold
  hosts$informal <- as.logical(
    as.character(
      cut(hosts$average, 
          breaks = c(-Inf, ref_threshold, Inf), 
          labels = c("FALSE", "TRUE"))))
  ## Calculate the proportion of source country refugees that live in informal host countries 
  hosts$informal <- hosts$informal * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$informal, na.rm = TRUE))
}

## Calculate % refugee
agg_df$pinformal_33 <- sapply(agg_df$case, FUN = PropInformal, ref_threshold = 33)
summary(agg_df$pinformal_33)

agg_df$pinformal_50 <- sapply(agg_df$case, FUN = PropInformal, ref_threshold = 50)
summary(agg_df$pinformal_50)

## Save File
save(agg_df, 
     file = "../Intermediate/Conflict case with refugee origin and host state information.RData")

print("05d")
