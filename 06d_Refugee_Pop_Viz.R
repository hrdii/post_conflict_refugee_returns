##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-11-19 for Post-Conflict Refugee return Project
## Description: Creates a graph of refugee and refugee return population overtime

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
library(ggplot2)
library(cowplot)
library(scales)
library(broom)

## Loading Refugee data
refugee_df <- read.csv("../Data/UNHCR/Refugees (incl. refugee-like situations).csv", 
                       skip = 3, header = T, stringsAsFactors = F)

## Clean refugee data
refugee_df <- setDT(refugee_df)
refugee_df$Value <- as.numeric(refugee_df$Value)

## Agregate refugee population by Year
refugee_df <- refugee_df[, .(ref_pop = sum(Value, na.rm = TRUE)), by = .(Year)]

## Load Refugee Returned data
rr_df <- read.csv("../Data/UNHCR/Returned refugees.csv", 
                  skip = 3, header = T, stringsAsFactors = F)

## Clean Refugee Returned data
rr_df <- setDT(rr_df)
rr_df$Value <- as.numeric(rr_df$Value)

## Agregate refugee returned population by Year
rr_df <- rr_df[, .(rr_pop = sum(Value, na.rm = TRUE)), by = .(Year)]

## Merge Data
setkey(refugee_df, "Year")
setkey(rr_df, "Year")
temp_df <- rr_df[refugee_df,]
temp_df <- rbindlist(list(temp_df, data.table(Year = 2019, rr_pop = 317181, ref_pop = 20444819)))

## Calculate proportion returned
temp_df$prop <- temp_df$rr_pop / temp_df$ref_pop

## Plot proportion of refugee returned 1950-2018 

for (sy in c(1950,1989)) {
  b1 <- sy + {sy %% 2}
  ggplot(temp_df)+
    geom_point(mapping = aes(x = Year, y = prop))+
    geom_segment(mapping = aes(x = Year, xend = Year, y = 0, yend = prop))+
    scale_y_continuous("Percent",
                       limits = c(0,0.25),
                       labels = scales::percent)+
    scale_x_continuous("Year",
                       breaks = seq(from = b1,
                                    to = 2020,
                                    by = 10),
                       limits = c(sy, 2020),
                       minor_breaks = TRUE)+
    theme_bw()
  save_plot(file = paste0("../Visualization/Refugee Population/prop refugee returned population", sy, "-2019.png"), 
            plot = last_plot())
}

temp_df <- temp_df[, c("Year", "ref_pop", "rr_pop")]

## reshape wide to long
temp_df <- reshape(temp_df, 
                   varying = c("ref_pop", "rr_pop"), 
                   v.names = "pop",
                   idvar = "Year",
                   timevar = "desc",
                   times = c("refugee", "rr"),
                   direction = "long",sep = "_")

## Total Refugee Population & Refugee Return 
for (sy in c(1950,1989)) {
  b1 <- sy + {sy %% 2}
  ggplot(temp_df)+
    geom_line(mapping = aes(x = Year,
                            y = pop,
                            linetype = desc))+
    scale_linetype_manual("",
                          values = c("refugee" = "solid",
                                     "rr" = "dashed"),
                          labels = c("refugee" = "Refugee",
                                     "rr" = "Refugee return"))+
    scale_y_continuous("Population (millions)",
                       labels = function(x) x/1000000,
                       minor_breaks = FALSE)+
    scale_x_continuous("Year",
                       breaks = seq(from = b1,
                                    to = 2020,
                                    by = 10),
                       limits = c(sy, 2020),
                       minor_breaks = TRUE)+
    theme_bw()+theme(legend.position="bottom")
  save_plot(file = paste0("../Visualization/Refugee Population/total refugee population", sy, "-2019.png"), 
            plot = last_plot())
}

print("6c")