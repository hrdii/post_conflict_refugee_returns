##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-11 for Post-Conflict Refugee Returns Project
## Description: Return Visualization

##### environment set up
rm(list = ls())
## Load Libraries
library(data.table)
library(ggplot2)
library(cowplot)
library(scales)
library(broom)

## Load Conflict Cases
load("../Intermediate/Conflict case with refugee origin and host state information.RData")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")
## Load Refugee Return Information
load("../Intermediate/Refugee Return Population.RData")
## Load Shortlist of Refugee Sources
load("../Intermediate/Shortlist of Major Refugee Sources.Rdata")

#####
## Refugee Population 1989-2018 from shortlisted sources

# Subset refugee dataset to shortlisted sources
# sources <- sort(unique(agg_df$country))
# temp_df <- refugee_df[source %in% sources, .(year, source, total)]
# 
# p <- list()
# 
# for (i in seq_along(sources)) {
#   p[[i]] <- ggplot(temp_df[temp_df$source == sources[i],])+
#     geom_point(mapping = aes(x = year, y = source, size = total))+
#     scale_y_discrete(name = "")+
#     scale_size_continuous(name = "",
#                           labels = scales::comma,
#                           breaks = c(0, 2e+6, 4e+6, 6e+6),
#                           limits = c(0, 6500000))+
#     scale_x_continuous(name = "", 
#                        breaks = c(seq(from = 1989,
#                                       to = 2018,
#                                       by = 10), 2018))+
#     theme_minimal()+guides(size = "none")+
#     theme(axis.line.x = element_blank(),
#           axis.text.x = element_blank(),
#           axis.ticks.x = element_blank())
# }
# 
# ref_pop <- plot_grid(plotlist = p, 
#                      nrow = length(p), 
#                      align = "v", 
#                      rel_widths = rep(1, length(p)))
# save_plot(plot = ref_pop, "Rplot.png", base_height = 10)

#####
## % Refugee Returned Graphs

temp_df <- agg_df[, c("country", "year0", paste0("pc1_10_rr_year", 1:10))]

temp_df$country[grep("Bosnia", temp_df$country)] <- "BiH"
temp_df$country[grep("Central", temp_df$country)] <- "CAR"
temp_df$country[grep("Democratic", temp_df$country)] <- "DR Congo"
temp_df$country[grep("Russia", temp_df$country)] <- "Russia Fed."
temp_df$country[grep("Serbia", temp_df$country)] <- "Serbia"


temp_df$case <- paste0(temp_df$country, 
                       "\n", 
                       temp_df$year0)
temp_df <- temp_df[, !{names(temp_df) %in% c("country", "year0")}]
temp_df <- reshape(temp_df, 
                   varying = paste0("pc1_10_rr_year", 1:10), 
                   v.names = "pc1_10_rr",
                   timevar = "year",
                   idvar = "case",
                   times = 1:10,
                   direction = "long",sep = "_")

## % Refugee Returned by case 
ggplot(temp_df)+
  geom_line(mapping = aes(x = year, y = pc1_10_rr))+
  facet_wrap(~case, ncol = 8)+
  geom_hline(yintercept = 0.5, linetype = "dashed")+
  scale_x_continuous(name = "Year since the end of conflict",
                     breaks = seq(from = 0, to = 10, by = 2),
                     minor_breaks = FALSE)+
  scale_y_continuous(name = "Proportion of refugees returned",
                     breaks = seq(from = 0, to = 1, by = 0.25),
                     limits = c(0, 1),
                     minor_breaks = FALSE,
                     labels = scales::percent)+theme_bw()+
  theme(text = element_text(size = 10))
save_plot(file = "../Visualization/Refugee Population/Proportion of refugees returned Over Year1-10.png", 
          plot = last_plot(), 
          base_width = 6.5,
          base_height = 7.5)

## % Refugee Returned by year 
ggplot(temp_df, mapping = aes(x = year, y = pc1_10_rr))+
  geom_jitter(width = 0.2, alpha = 0.7)+
  geom_smooth(se = FALSE, color = "black")+
  scale_x_continuous("Year since the end of conflict",
                     breaks = seq(from = 0,
                                  to = 10,
                                  by = 2),
                     limits = c(0.5, 10.5))+
  scale_y_continuous("Proportion of refugees returned",
                     limits = c(-0.01,1),
                     labels = scales::percent)+
  theme_bw()

save_plot(file = "../Visualization/Refugee Population/Prop Refugees Returned by year.png", 
          plot = last_plot())

## % Refugee Returned by year boxplot
temp_df$year <- factor(temp_df$year)

ggplot(temp_df)+
  geom_boxplot(mapping = aes(year, pc1_10_rr))+
  scale_x_discrete("Year since the end of conflict")+
  scale_y_continuous("Proportion of refugees returned",
                     limits = c(0,1),
                     labels = scales::percent)+
  theme_bw()

save_plot(file = "../Visualization/Refugee Population/Prop Refugees Returned by year boxplot.png", 
          plot = last_plot())

## % Refugee Returned by year by success
temp_df$year <- as.numeric(as.character(temp_df$year))

success <- temp_df$pc1_10_rr[temp_df$year == 10] >= 0.5
temp_df$success <- as.character(rep(success, 10))

mod1 <- lm(pc1_10_rr ~ year, data = temp_df[temp_df$success == "TRUE", ])
mod2 <- lm(pc1_10_rr ~ year, data = temp_df[temp_df$success == "FALSE", ])
m <- data.frame(success = c("TRUE", "FALSE"),
                slope = c(coef(mod1)[2], coef(mod2)[2]))
m$slope <- paste0("m = ", round(m$slope, 3))

ggplot(temp_df, mapping = aes(x = year, 
                              y = pc1_10_rr))+
  geom_jitter(width = 0.2, alpha = 0.5)+
  geom_smooth(se = FALSE, color = "black")+
  geom_smooth(method = "lm",
              se = FALSE,
              linetype = "dashed", color = "black")+
  facet_grid(~success,
             labeller = labeller(success = c("FALSE" = "Less than 50% returns",
                                             "TRUE" = "More than 50% returns")))+
  geom_text(data = m, 
            mapping = aes(x = 2, y = 0.85, label = slope))+
  scale_x_continuous("Year since the end of conflict",
                     limits = c(0,10.5),
                     breaks = seq(from =0,
                                  to = 10,
                                  by = 2))+
  scale_y_continuous("Proportion of refugees returned",
                     limits = c(-0.01,1),
                     labels = scales::percent)+
  theme_bw()+theme(legend.position = "none")

save_plot(file = "../Visualization/Refugee Population/Prop Refugees Returned by year and by success.png", 
          plot = last_plot())


print("6b")