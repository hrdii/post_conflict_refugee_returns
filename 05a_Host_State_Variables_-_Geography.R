##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-09 for Post-Conflict Refugee Returns Project
## Description: Adds Host state variables to conflict cases 
  ## Geography
    ## % refugees at the end of conflict that are in host countries that share 
	## a land border with source country


##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
library(spdep)
library(tmap)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Load Conflict case with refugee and origin state information
load("../Intermediate/Conflict case with refugee and origin state information.RData")
## Load Shortlist of Refugee Sources
load("../Intermediate/Shortlist of Major Refugee Sources.RData")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")
## Get shape files for all countries in the world
data(world)

##### Country Name Harmonization 

## Create a list of all Countries in the World Map
world$name_long <- NameCleaning(world$name_long)

## Create a list of all Host Countries
host_countries <- names(refugee_df)
host_countries <- setdiff(host_countries, c("year", "source", "total"))

## All the countries in the world map not matched with host countries list
lookup_df <- data.table(map_names = setdiff(world$name_long, host_countries))

## Matches for countries in the world map from host countries list
lookup_df$unhcr_names <- NA
lookup_df$unhcr_names[lookup_df$map_names == "Tanzania"] <- "United Republic Tanzania"
lookup_df$unhcr_names[lookup_df$map_names == "United States"] <- "United States America"
lookup_df$unhcr_names[lookup_df$map_names == "Bolivia"] <- "Bolivia Plurinational State"
lookup_df$unhcr_names[lookup_df$map_names == "Venezuela"] <- "Venezuela Bolivarian Republic"
lookup_df$unhcr_names[lookup_df$map_names == "Côte d Ivoire"] <- "Ivory Coast"
lookup_df$unhcr_names[lookup_df$map_names == "Republic Congo"] <- "Congo"
lookup_df$unhcr_names[lookup_df$map_names == "Lao PDR"] <- "Lao People s Democratic Republic"
lookup_df$unhcr_names[lookup_df$map_names == "Democratic Republic Korea"] <- "Republic Korea"
lookup_df$unhcr_names[lookup_df$map_names == "Syria"] <- "Syrian Arab Republic"
lookup_df$unhcr_names[lookup_df$map_names == "Moldova"] <- "Republic Moldova"
lookup_df$unhcr_names[lookup_df$map_names == "Macedonia"] <- "former Yugoslav Republic Macedonia"
lookup_df$unhcr_names[lookup_df$map_names == "Serbia"] <- "Serbia Kosovo S RES"

## All the host countries that are still unmatched with countries in the world map 
temp <- setdiff(host_countries, world$name_long)
temp <- setdiff(temp, lookup_df$unhcr_names)

lookup_df <- rbind(lookup_df, data.table(map_names = NA,
                                       unhcr_names = temp))
lookup_df <- lookup_df[complete.cases(lookup_df)]

rm(temp)

## Replace country names in the world map
world$name_long[which(world$name_long %in% lookup_df$map_names)] <- lookup_df$unhcr_names

rm(lookup_df)

##### Calculate % refugee

## Write a function to calculate % refugees
PropAdjacent <- function(cas, r_df = refugee_df, all_countries = world$name_long, hood = nb){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset Refugee Data to Year0
  temp_df <- r_df[year == y, ]
  ## Subset to host that are above the threshold
  hosts <- SubsetHosts(country = s, df = temp_df)
  ## Create a list of neighbors
  neighbor_list <- all_countries[hood[[which(all_countries == s)]]]
  ## Add neighbor status for each host country
  hosts$neighbor <- hosts$country %in% neighbor_list
  ## Calculate the proportion of source country refugees that live in neighboring host countries 
  hosts$neighbor <- hosts$neighbor * hosts$pop / sum(hosts$pop, na.rm = TRUE)
  ## Return proportion
  return(sum(hosts$neighbor, na.rm = TRUE))
}

## Create list of neighboring countries for every country
nb <- spdep::poly2nb(world, queen = TRUE)

## Calculate % refugee
agg_df$pneighbor <- sapply(agg_df$case, FUN = PropAdjacent)

summary(agg_df$pneighbor)

## Save File
save(agg_df, 
     file = "../Intermediate/Conflict case with refugee origin and host state information.RData")

print("05a")