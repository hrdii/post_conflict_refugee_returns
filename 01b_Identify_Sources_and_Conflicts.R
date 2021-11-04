##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-01-30 for Post-Conflict Refugee Returns Project
## Description: Cleans UCDP-PRIO Georeferenced Event Dataset to create an 
	## aggregated dataset 

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
## Load Functions
source(file = "00a_Custom_Functions.R")

## UNHCR sources
#ref_source <- c("Afghanistan",  "Angola",  "Armenia",  "Azerbaijan",  "Bhutan",  "Bosnia and H.",  "Burundi",  "Cambodia",  "Central African Rep.",  "Chad",  "China",  "Colombia",  "Côte d’Ivoire",  "Croatia",  "Dem Rep of Congo",  "El Salvador",  "Equatorial Guinea",  "Eritrea",  "Ethiopia",  "Guatemala",  "Iran",  "Iraq",  "Lao",  "People’s Dem Rep Liberia",  "Mali",  "Mauritania",  "Mozambique",  "Myanmar",  "Namibia",  "Nicaragua",  "Nigeria",  "Pakistan",  "Philippines",  "Russian Federation",  "Rwanda",  "Serbia",  "Sierra Leone",  "Somalia",  "South Africa",  "South Sudan",  "Sri Lanka",  "Sudan",  "Syrian Arab Rep.",  "Timor-Leste",  "Togo",  "Turkey",  "Uganda",  "Ukraine",  "Unknown origin",  "Viet Nam",  "Western Sahara",  "Yemen")
## Major Cources with population greater than 10000
load(file = "../Intermediate/Longlist of Major Refugee Sources.RData")

## Load the geo-referenced event dataset
load("../Data/UCDP Georeferenced Event Dataset/ged191.RData")

##### Basic Cleaning and Aggregation

## Subset to countries, year, best estimate of fatalities and whether it was an active conflict year
ged191 <- setDT(ged191[, c("year", "country", "country_id", "best", "active_year", "type_of_violence")])
ged191$type_of_violence <- as.character(ged191$type_of_violence)
## Aggregate the number of deaths, event count and active year status by year and country
agg_df <- ged191[, 
                 .(deaths = sum(best), event_count = .N), 
                 by = .(year, country, type_of_violence)]

## Encode types of violence 
lookup_df <- data.table(old = c("1", "2", "3"), 
                        new = c("state",
                                "nonstate",
                                "onesided"))
agg_df[lookup_df, on=.(type_of_violence = old), type_of_violence := i.new ]

rm(lookup_df)

## Reshape data for type of violence 
agg_df <- reshape(agg_df, 
                           v.names=c("deaths", "event_count"), 
                           timevar="type_of_violence", 
                           idvar=c("year", "country"),
                           direction="wide")
names(agg_df) <- gsub("\\.", "_", names(agg_df))
## Impute zeros for missing values
agg_df[is.na(agg_df)] <- 0

## Calculate total deaths and event counts
agg_df <- agg_df[, deaths_total := rowSums(.SD), .SDcol = grep("deaths", names(agg_df))]
agg_df <- agg_df[, event_count_total := rowSums(.SD), .SDcol = grep("event_count", names(agg_df))]

## Drop cases where death toll is less than 25 
agg_df <- agg_df[agg_df$deaths_total >= 25]
agg_df$country <- NameCleaning(agg_df$country)



##### Narrowing down to conflict cases from major refugee sources

## Harmozining Country Names
lookup_df <- data.table(ref_names = setdiff(ref_source, agg_df$country))

lookup_df <- lookup_df[, db_names := NA]
lookup_df$db_names[lookup_df$ref_names == "Democratic Republic Congo"] <- "DR Congo Zaire"
lookup_df$db_names[lookup_df$ref_names == "Lao People s Democratic Republic"] <- "Laos"
lookup_df$db_names[lookup_df$ref_names == "Cambodia"] <- "Cambodia Kampuchea"
lookup_df$db_names[lookup_df$ref_names == "Russian Federation"] <- "Russia Soviet Union"
lookup_df$db_names[lookup_df$ref_names == "Yemen"] <- "Yemen North Yemen"
lookup_df$db_names[lookup_df$ref_names == "Myanmar"] <- "Myanmar Burma"
lookup_df$db_names[grep("Serbia", lookup_df$ref_names)] <- "Serbia Yugoslavia"
lookup_df$db_names[grep("Macedonia", lookup_df$ref_names)] <- "Macedonia FYR"
lookup_df$db_names[grep("Moldova", lookup_df$ref_names)] <- "Moldova"
lookup_df$db_names[grep("Zimbabwe", lookup_df$ref_names)] <- "Zimbabwe Rhodesia"
lookup_df$db_names[grep("Venezuela", lookup_df$ref_names)] <- "Venezuela"

lookup_df <- lookup_df[complete.cases(lookup_df)]

agg_df[lookup_df, on=.(country = db_names), country := i.ref_names]

## Save File
fwrite(agg_df, file = "../Intermediate/Cases with Active Conflict.csv")

## Subset to countries that are major refugee sources 
agg_df <- agg_df[country %in% ref_source]
major_source <- intersect(ref_source, agg_df$country)

## Create a variable for start year to calculate length of war later
agg_df <- agg_df[, start_year := min(year), by = .(country)]

## Save files  
save(major_source, file = "../Intermediate/Shortlist of Major Refugee Sources.RData")
write.csv(agg_df, "../Output/All Conflict Cases with deaths 1989-2018.csv")

print("1b")