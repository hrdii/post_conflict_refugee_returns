##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-10 for Post-Conflict Refugee Returns Project
## Description: Create a dataframe with Historical Income Classification Thresholds

##### environment set up
rm(list = ls())
## Load Libraries
library(readxl)
## Load Functions
source(file = "00a_Custom_Functions.R")

class_df <- data.frame(year = 1987, low = 480, low_mid = 1940, up_mid = 6000)
class_df <- rbind(class_df, data.frame(year = 1988, low = 545, low_mid = 2200, up_mid = 6000))
class_df <- rbind(class_df, data.frame(year = 1989, low = 580, low_mid = 2335, up_mid = 6000))
class_df <- rbind(class_df, data.frame(year = 1990, low = 610, low_mid = 2465, up_mid = 7620))
class_df <- rbind(class_df, data.frame(year = 1991, low = 635, low_mid = 2555, up_mid = 7910))
class_df <- rbind(class_df, data.frame(year = 1992, low = 675, low_mid = 2695, up_mid = 8355))
class_df <- rbind(class_df, data.frame(year = 1993, low = 695, low_mid = 2785, up_mid = 8625))
class_df <- rbind(class_df, data.frame(year = 1994, low = 725, low_mid = 2895, up_mid = 8955))
class_df <- rbind(class_df, data.frame(year = 1995, low = 765, low_mid = 3035, up_mid = 9385))
class_df <- rbind(class_df, data.frame(year = 1996, low = 785, low_mid = 3115, up_mid = 9645))
class_df <- rbind(class_df, data.frame(year = 1997, low = 785, low_mid = 3125, up_mid = 9655))
class_df <- rbind(class_df, data.frame(year = 1998, low = 760, low_mid = 3030, up_mid = 9360))
class_df <- rbind(class_df, data.frame(year = 1999, low = 755, low_mid = 2995, up_mid = 9265))
class_df <- rbind(class_df, data.frame(year = 2000, low = 755, low_mid = 2995, up_mid = 9265))
class_df <- rbind(class_df, data.frame(year = 2001, low = 745, low_mid = 2975, up_mid = 9205))
class_df <- rbind(class_df, data.frame(year = 2002, low = 735, low_mid = 2935, up_mid = 9075))
class_df <- rbind(class_df, data.frame(year = 2003, low = 765, low_mid = 3035, up_mid = 9385))
class_df <- rbind(class_df, data.frame(year = 2004, low = 825, low_mid = 3255, up_mid = 10065))
class_df <- rbind(class_df, data.frame(year = 2005, low = 875, low_mid = 3465, up_mid = 10725))
class_df <- rbind(class_df, data.frame(year = 2006, low = 905, low_mid = 3595, up_mid = 11115))
class_df <- rbind(class_df, data.frame(year = 2007, low = 935, low_mid = 3705, up_mid = 11455))
class_df <- rbind(class_df, data.frame(year = 2008, low = 975, low_mid = 3855, up_mid = 11905))
class_df <- rbind(class_df, data.frame(year = 2009, low = 995, low_mid = 3945, up_mid = 12195))
class_df <- rbind(class_df, data.frame(year = 2010, low = 1005, low_mid = 3975, up_mid = 12275))
class_df <- rbind(class_df, data.frame(year = 2011, low = 1025, low_mid = 4035, up_mid = 12475))
class_df <- rbind(class_df, data.frame(year = 2012, low = 1035, low_mid = 4085, up_mid = 12615))
class_df <- rbind(class_df, data.frame(year = 2013, low = 1045, low_mid = 4125, up_mid = 12745))
class_df <- rbind(class_df, data.frame(year = 2014, low = 1045, low_mid = 4125, up_mid = 12735))
class_df <- rbind(class_df, data.frame(year = 2015, low = 1025, low_mid = 4035, up_mid = 12475))
class_df <- rbind(class_df, data.frame(year = 2016, low = 1005, low_mid = 3955, up_mid = 12235))
class_df <- rbind(class_df, data.frame(year = 2017, low = 995, low_mid = 3895, up_mid = 12055))
class_df <- rbind(class_df, data.frame(year = 2018, low = 1025, low_mid = 3995, up_mid = 12375))

## Save File  
save(class_df, file = "../Intermediate/Historical GNI Per Capita Classification.RData")


inc_df <- read_excel("../Data/World Bank/Income/Historical classifications of economies by income.xls", 
                     sheet = "Country Analytical History", 
                     na = "..", skip = 5)
inc_df <- inc_df[!{seq_along(rownames(inc_df)) %in% 1:5}, !{seq_along(names(inc_df)) %in% 1}]
names(inc_df)[1] <- "country"

## Function to identify rows with missing data
MissingRow <- function(x){
  # x is a matrix or dataframe
  n <- ncol(x) - 1
  y <- is.na(x)
  y <- apply(y, MARGIN = 1, FUN = sum)
  y <- y >= n
  return(y)
}

## Drop missing rows
temp <- MissingRow(inc_df)
inc_df <- inc_df[!temp, ]

rm(temp)

## Other Cleaning
inc_df$country <- NameCleaning(inc_df$country)

save(inc_df, file = "../Intermediate/GNI Per Capita.RData")

print("00b")
