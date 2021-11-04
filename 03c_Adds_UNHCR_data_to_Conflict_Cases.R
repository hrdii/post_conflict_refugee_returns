##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-01-30 for Post-Conflict Refugee Returns Project
## Description: Adds UNHCR data to conflict cases and calculates % returned 

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## Loading Refugee Data
r_df <- fread("../Output/Refugee Population from shortlisted sources to all hosts 1989-2018.csv",
              drop = 1)
## Loading Refugee Returned Data
rr_df <- fread("../Output/Refugee Returned Population from shortlisted sources to all hosts 1989-2018.csv",
              drop = 1)
## Conflict case dataset
load("../Intermediate/Conflict Cases.RData")

##### Add UNHCR data for years 0-10

## Create Years0-10 for each conflict
temp <- lapply(agg_df$year, FUN = function(x, i) x + i, 1:10)
temp <- as.data.table(t(data.frame(temp)))
agg_df <- agg_df[, paste0("year", 1:10) := temp]
setnames(agg_df, "year", "year0")

rm(temp)

## Create a function to add UNHCR data for years 0-10
MyMerge <- function(cas, df){
  ## Source Country
  s <- gsub('(.*) ([0-9]{4})','\\1',cas)
  ## Year0
  y <- as.numeric(gsub('(.*) ([0-9]{4})','\\2',cas))
  ## Subset refugee dataset to source country between Year0-10
  x <- df[source == s & year %in% y:{y+11}, total]
  ## Force the length to 11
  length(x) <- 11
  return(x)
}

## Adding Refugee data for years 0-10
temp <- lapply(agg_df$case, FUN = MyMerge, df = r_df)
temp <- as.data.table(t(data.frame(temp)))

## Dropping cases where refugee stock never rises to more than 10,000 in Year0-10
to_keep <- apply(temp, FUN = max, MARGIN = 1, na.rm = TRUE) >= 10000
temp <- cbind(temp, to_keep)
agg_df[, c(paste0("ref_year", 0:10), "to_keep") := temp]
agg_df <- agg_df[to_keep]
agg_df <- agg_df[, !"to_keep"]

rm(temp)
rm(to_keep)

## Dropping cases where refugee stock is less than 1000 in Year0
agg_df <- agg_df[ref_year0 >= 1000]

## Adding Refugee Returned data for years 0-10
temp <- lapply(agg_df$case, FUN = MyMerge, df = rr_df)
temp <- as.data.table(t(data.frame(temp)))
agg_df[, paste0("rr_year", 0:10) := temp]

rm(temp)

## Imputing missing values for conflicts that ended before 2009
agg_df[is.na(agg_df) & year0 < 2009] <- 0

##### Calculate Refugee Flows

## Calculate New Refugees
agg_df <- setDF(agg_df)
for (i in 1:10) {
  agg_df[, paste0("nref_year", i)] <- 
    agg_df[, paste0("ref_year", i)] + 
    agg_df[, paste0("rr_year", i)] - 
    agg_df[, paste0("ref_year", {i-1})]
}

## Setting Negative "new refugees" equal to 0
vars <- paste0("nref_year", 1:10)
agg_df[, vars][agg_df[, vars] < 0] <- 0

rm(vars)

## Calculating Composite Cumulative Refugee Population
agg_df$cref_year1 <- agg_df$ref_year0 + agg_df$nref_year1 

for (i in 2:10) {
  agg_df[, paste0("cref_year", i)] <- 
    agg_df[, paste0("cref_year", {i-1})] +
    agg_df[, paste0("nref_year", i)]
}

## Calculating Cumulative Refugee Returned Population from Year1-10
vars <- paste0("rr_year", 1:10)
temp <- data.frame(t(apply(agg_df[, vars], FUN = cumsum, MARGIN = 1)))
names(temp) <- paste0("c1_10_",names(temp))

## Combining Cumulative Refugee Returned
agg_df <- cbind(agg_df, temp)

## Calculating Cumulative Refugee Returned Population from Year0-10
vars <- paste0("rr_year", 0:10)
temp <- data.frame(t(apply(agg_df[, vars], FUN = cumsum, MARGIN = 1)))
names(temp) <- paste0("c0_10_",names(temp))

## Combining Cumulative Refugee Returned
agg_df <- cbind(agg_df, temp)

## Removing temporary variables
rm(temp)
rm(vars)

## Creating % Cumulative Refugee Returned from Year1-10
agg_df[, paste0("pc1_10_rr_year", 1:10)] <- 
  agg_df[, paste0("c1_10_rr_year", 1:10)] /
  agg_df[, paste0("cref_year", 1:10)]

## Creating % Cumulative Refugee Returned from Year01-10
agg_df[, paste0("pc0_10_rr_year", 0)] <- 
  agg_df[, paste0("c0_10_rr_year", 0)] /
  agg_df[, paste0("ref_year", 0)]

agg_df[, paste0("pc0_10_rr_year", 1:10)] <- 
  agg_df[, paste0("c0_10_rr_year", 1:10)] /
  agg_df[, paste0("cref_year", 1:10)]

## Save File 
write.csv(agg_df, file = paste0("../Output/Updated ", Sys.Date(), " merged db with refugee information.csv"))
save(agg_df, file = "../Intermediate/Conflict case with refugee information.RData")

print("3c")