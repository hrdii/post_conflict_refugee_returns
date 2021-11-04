##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-01-31 for Post-Conflict Refugee Returns Project
## Description: Defines custom functions to calculate Host state variables 

##### SubsetHosts

SubsetHosts <- function(country = country, df = temp_df, threshold = 0.995){
  ## subset to data from source country only
  hosts <- as.data.frame(t(df[source == country, !c("year", "source", "total")]))
  names(hosts) <- "pop"
  hosts$country <- rownames(hosts)
  ## Arrange all host countries in descending order of refugees from source country
  hosts <- hosts[rev(order(hosts$pop)),]
  ## Create a cumulative count of refugees 
  hosts$cumprop <- cumsum(hosts$pop)
  ## Create cumulative proportion of refugees 
  hosts$cumprop <- hosts$cumprop / sum(hosts$pop, na.rm = TRUE)
  ## Drop host countries that have less than threshold cumulative proportion of refugees
  index <- which(hosts$cumprop > threshold)[1]
  hosts <- hosts[1:index,]
  ## Clean host country names
  hosts$country <- tm::removePunctuation(hosts$country)
  
  return(hosts)
}

##### NameCleaning
NameCleaning <- function(x){
  ## Remove Punctuation
  x <- gsub("[[:punct:]]", " ", x)
  ## Remove Digits
  x <- gsub("[[:digit:]]", " ", x)
  ## Replace Republic 
  x <- gsub("Rep($| )", "Republic ", x)
  ## Replace Democratic
  x <- gsub("Dem($| )", "Democratic ", x)
  ## Remove The
  x <- gsub("( |^)(T|t)he", " ", x)
  ## Remove and / of
  x <- gsub(" (and|of)", " ", x)
  ## Remove Excess Whitespace
  x <- stringr::str_squish(x)
  return(x)
}