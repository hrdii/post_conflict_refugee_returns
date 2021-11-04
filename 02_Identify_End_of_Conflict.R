##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-01-30 for Post-Conflict Refugee Returns Project
## Description: Takes aggregated conflict cases dataset and determines end of conflict 

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load aggregated conflict cases 
agg_df <- fread("../Output/All Conflict Cases with deaths 1989-2018.csv", drop = 1)

## Load List of Major Refugee Sources
load(file = "../Intermediate/Shortlist of Major Refugee Sources.RData")

##### Identifying ends of conflict

## Create a variable for intensity of conflict
agg_df <- agg_df[, high_intensity := deaths_total >= 1000]

## Subset to cases with high intensity conflict
agg_df <- agg_df[high_intensity == TRUE]

## Record the ends of conflict
agg_df <- agg_df[, last_record := year == max(year), by = .(country)]

sum(agg_df$last_record, na.rm = TRUE)

## Record any other ends of conflict
agg_df <- agg_df[order(year)]
agg_df <- agg_df[, other_record := abs(year - shift(year, type = "lead"))> 2, by = .(country)]

sum(agg_df$other_record, na.rm = TRUE)

## combine all ends of conflict
agg_df <- agg_df[, all_records := rowSums(.SD, na.rm = TRUE) > 0, .SDcol = c("last_record", "other_record")]

sum(agg_df$all_records, na.rm = TRUE)

## For output
# agg_df <- agg_df[, year0 := as.numeric(all_records)]
# agg_df <- agg_df[, !c("X", "high_intensity", "last_record", "other_record", "all_records")]
# write.csv(agg_df, "../Output/Shortlisted Conflict Cases 1989-2018.csv")

## Subset to all ends of conflict
agg_df <- agg_df[all_records == TRUE]

## Drop ends of conflict that are too close to each other  
agg_df <- agg_df[order(year)]
agg_df <- agg_df[, temp := abs(year - shift(year, type = "lead"))> 2, by = .(country)]
agg_df <- agg_df[is.na(temp), temp := TRUE]
agg_df <- agg_df[temp == TRUE]

## Drop ends of conflict that are too recent (2017, and 2018)  
agg_df <- agg_df[year<2009]

## drop extra variables
agg_df <- agg_df[, !c("high_intensity", "last_record", "other_record", "all_records","temp")]

## Create Case variable by combining country and year
agg_df <- agg_df[, case := paste(country, year)]

save(agg_df, file = "../Intermediate/Conflict Cases.RData")
write.csv(agg_df, "../Output/Longlisted Conflict Cases with deaths in Y0.csv")

print("2")