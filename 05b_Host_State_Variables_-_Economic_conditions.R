##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-09 for Post-Conflict Refugee Returns Project
## Description: Adds Host state variables to conflict cases 
  ## Economic conditions
    ## Distribution of refugees at the end of conflict living across host 
	## countries with different income levels. Based on GNI per capita and income
	## classification defined by World Bank. 

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Load Conflict Cases
load("../Intermediate/Conflict case with refugee origin and host state information.RData")
## Load WB-UNHCR Country Name Lookup Dataframe
load("../Intermediate/WB-UNHCR Country Name Lookup.RData")
## Historical GNI Classification Thresholds
load("../Intermediate/GNI Per Capita.RData")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")

host_countries <- names(refugee_df)
host_countries <- setdiff(host_countries, c("year", "source", "total"))

inc_df <- setDT(inc_df)
## Replace Country names using Lookup Dataframe
inc_df[lookup_df, on=.(country = wb_names), country := i.unhcr_names ]

rm(lookup_df)
## Reshape inc_df
inc_df <- melt(inc_df, id.vars = c("country"),
               measure.vars = as.character(1989:2018))
names(inc_df) <- c("country", "year", "inc_group")
inc_df$year <- as.numeric(as.character(inc_df$year))

##Imputing specific missing values
inc_df$inc_group[grepl("Serbia", inc_df$country) & is.na(inc_df$inc_group)] <- "LM"


##### Calculate % refugee

## Write a function to calculate % refugees
PropInc <- function(cas, inc_group, r_df = refugee_df, i_df = inc_df){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to host that are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Subset income classification to year0 
  i_df <- i_df[year == y, c("country", "inc_group")]
  ## Add Income classification for all hosts
  hosts <- merge(hosts, i_df, by.x=c("country"), by.y=c("country"), all.x = TRUE)
  hosts$inc_group <- hosts$inc_group %in% inc_group
  ## Calculate the proportion of source country refugees that live in host countries in a given income group
  hosts$inc_group <- hosts$inc_group * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$inc_group, na.rm = TRUE))
}

## Calculate % refugee living in low income countries
agg_df$pinc_low <- sapply(agg_df$case, FUN = PropInc, inc_group = "L")
## Calculate % refugee living in middle income countries
agg_df$pinc_middle <- sapply(agg_df$case, FUN = PropInc, inc_group = c("LM", "UM"))
## Calculate % refugee living in high income PropInc
agg_df$pinc_high <- sapply(agg_df$case, FUN = PropInc, inc_group = "H")

## Calculate % refugee living in countries with recorded income
agg_df$pinc <- rowSums(agg_df[, c("pinc_low", 
                                  "pinc_middle", 
                                  "pinc_high")], na.rm = TRUE)

## Calculate % refugee living in countries without recorded income
agg_df$pinc <- pmax({1 - agg_df$pinc}, 0) 
## Proportion of refugees living in host countries for whom income data exists doesn't 
## add up to one in some cases. The only explanation for that could be that refugees 
## are living in countries that are war-torn themselves. These countries are unlikely 
## to be high-income. Therefore residual proportion is added to low-income variable.
agg_df$pinc_low <- agg_df$pinc_low + agg_df$pinc
## Dropping the pinc variable
agg_df <- agg_df[, !{names(agg_df) %in% "pinc"}]

summary(agg_df$pinc_low)
summary(agg_df$pinc_middle)
summary(agg_df$pinc_high)

## Save File
save(agg_df, 
     file = "../Intermediate/Conflict case with refugee origin and host state information.RData")

print("05b")
