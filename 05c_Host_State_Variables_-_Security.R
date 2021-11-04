##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-10 for Post-Conflict Refugee Returns Project
## Description: Adds Host state variables to conflict cases 
  ## Security
    ## % refugees at the end of conflict that are in host countries that are 
	## considered insecure. A host country might be considered insecure if it 
	## has >=10,000 refugees in the year the conflict ends

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Load Conflict Cases
load("../Intermediate/Conflict case with refugee origin and host state information.RData")
## Read Conflict Data for all active cases
conflict_df <- fread(file = "../Intermediate/Cases with Active Conflict.csv")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")

## Create a list of all Host Countries
host_countries <- names(refugee_df)
host_countries <- setdiff(host_countries, c("year", "source", "total"))

## Subset to cases with more than 1000 deaths
#conflict_df <- conflict_df[deaths_total >= 1000, c("country", "year", "deaths_total")]

lookup_df <- data.table(con_name = setdiff(unique(conflict_df$country), host_countries))

rm(lookup_df)

##### Calculate % refugee

## Write a function to calculate % refugees
PropHighRef <- function(cas, r_df = refugee_df){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to host that are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Add Refugee data
  temp_df <- temp_df[, c("source", "total")]
  hosts <- merge(x = hosts, y = temp_df, by.x = "country", by.y = "source", all.x = TRUE)
  ## Hosts are considered insecure if they have more than 10000 refugees in Year0
  hosts$total <- hosts$total >= 10000
  ## Calculate the proportion of source country refugees that live in insecure host countries 
  hosts$total <- hosts$total * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$total, na.rm = TRUE))
}

PropHighCasYr0 <- function(cas, r_df = refugee_df, c_df = conflict_df){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to host that are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Subset Casualty Data
  c_df <- c_df[year == y & deaths_total >= 1000, country]
  ## Hosts are considered insecure if they high intensity conflict in Year0
  hosts$highcas <- hosts$country %in% c_df
  ## Calculate the proportion of source country refugees that live in insecure host countries 
  hosts$highcas <- hosts$highcas * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$highcas, na.rm = TRUE))
}

PropSomeCasYr010 <- function(cas, r_df = refugee_df, c_df = conflict_df){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to host that are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Subset Casualty Data
  c_df <- c_df[year %in% y:{y+10}, .(count = .N), by = country]
  ## Hosts are considered insecure if they high intensity conflict in Year0
  hosts <- merge(x = hosts, y = c_df, by.x = "country", by.y = "country", all.x = TRUE)
  hosts$count <- hosts$count >=5
  ## Calculate the proportion of source country refugees that live in insecure host countries 
  hosts$count <- hosts$count * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$count, na.rm = TRUE))
}

## Calculate % refugee
agg_df$phighref <- sapply(agg_df$case, FUN = PropHighRef)
agg_df$phighcasyr0 <- sapply(agg_df$case, FUN = PropHighCasYr0)
agg_df$psomecasyr010 <- sapply(agg_df$case, FUN = PropSomeCasYr010)

summary(agg_df$phighref)
summary(agg_df$phighcasyr0)
summary(agg_df$psomecasyr010)

## Save File
save(agg_df, 
     file = "../Intermediate/Conflict case with refugee origin and host state information.RData")

print("05c")
