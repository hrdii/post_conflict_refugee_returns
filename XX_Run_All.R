##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-13 for Post-Conflict Refugee Returns Project
## Description: Runs a whole batch of R scripts

## Detach all packages 
other_pkg <- names(sessionInfo()$otherPkgs)

lapply(paste0("package:", other_pkg),
       detach, character.only=TRUE,unload=TRUE)

setwd("./refugee-returns")
## List All files
scripts <- list.files()
## Retain only R scripts
scripts <- scripts[grep(".R$", scripts)]
View(scripts)

## Run all the scripts
sapply(scripts[1:19], source)

## Write out package bibliography
knitr::write_bib(c(.packages(), 'rmarkdown', 'knitr'), 'packages.bib')
