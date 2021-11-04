##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-10 for Post-Conflict Refugee Returns Project
## Description: Adds Host state variables to conflict cases 
  ## Opportunity for education
    ## % refugees at the end of conflict that are in host countries with low 
	## education spending. Global average government spending on education as a 
	## percentage of GDP has varied between 4.4 and 4.7% over 1999-2015. A host 
	## country is arbitrarily defined as having low education spending if 
	## government spending on education as a percentage of GDP is less than 4% 
 
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

## Import Education Spending Data
ed_df <- fread("../Data/World Bank/Education/API_SE.XPD.TOTL.GD.ZS_DS2_en_csv_v2_422116.csv", header = TRUE, skip = 4)

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
temp <- MissingIndex(ed_df, row = TRUE, n = 4)
ed_df <- ed_df[!temp]

## Drop completely empty columns
temp <- MissingIndex(ed_df, row = FALSE, n = 0)
ed_df <- ed_df[, .SD, .SDcols=-temp]

rm(temp)
## Drop other unnecessary columns
ed_df <- ed_df[, !c("Country Code", "Indicator Name", "Indicator Code")]

## Clean names
setnames(ed_df, "Country Name", "country")

##### Harmonize Country Names

ed_df$country <- NameCleaning(ed_df$country)

## Replace Country names using Lookup Dataframe
ed_df[lookup_df, on=.(country = wb_names), country := i.unhcr_names ]

rm(lookup_df)

##### Calculate % refugee

## Write a function to calculate % refugees
PropLowEdSpend <- function(cas, r_df = refugee_df, econ_df = ed_df, ref_threshold = 4){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to sources that host are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Subset education spending to year0
  econ_df <- econ_df[, .SD, .SDcols= c("country", as.character(y))]
  names(econ_df) <- c("country", "edspend") 
  ## Add education spending for each host country
  hosts <- merge(x = hosts,
                 y = econ_df,
                 by = "country",
                 all.x = TRUE)
  ## Hosts are considered to have low education spending as a % of GDP if it is less than ref_threshold
  hosts$low_edspend <- as.logical(
    as.character(
      cut(hosts$edspend, 
          breaks = c(-Inf, ref_threshold, Inf), 
          labels = c("TRUE", "FALSE"))))
  ## Calculate the proportion of source country refugees that live in host countries with low education spending
  hosts$low_edspend <- hosts$low_edspend * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$low_edspend, na.rm = TRUE))
}

## Calculate % refugee
agg_df$pedspend_4 <- sapply(agg_df$case, FUN = PropLowEdSpend, ref_threshold = 4)
summary(agg_df$pedspend_4)

agg_df$pedspend_3 <- sapply(agg_df$case, FUN = PropLowEdSpend, ref_threshold = 3)
summary(agg_df$pedspend_3)

agg_df$pedspend_2 <- sapply(agg_df$case, FUN = PropLowEdSpend, ref_threshold = 2)
summary(agg_df$pedspend_2)

## Save File
save(agg_df, 
     file = "../Intermediate/Conflict case with refugee origin and host state information.RData")
file_path <- paste0("../Output/Updated ", Sys.Date(), " merged db with origin & host variables.csv")
fwrite(agg_df, file = file_path)

print("05f")