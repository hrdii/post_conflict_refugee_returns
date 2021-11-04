##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-11 for Refugee Returned Project
## Description: Exploratory Visualization 

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
## Distribution of Independent Variables
agg_df$country[grep("Serbia", agg_df$country)] <- "Serbia"
agg_df$case <- paste(agg_df$country, agg_df$year0)

p <- list()

vars <- c("cas25_year", "cas500_year", "cas1k_year", "length_war")

for (i in seq_along(vars)) {
  p[[i]] <- ggplot(agg_df, aes_string(vars[i]))+
    geom_histogram(binwidth = 1)+
    theme_bw()+ylab("Count")
  save_plot(file = paste0("../Visualization/Distributions & Densities/Distribution of ", vars[i], ".png"), plot = p[[i]])
}

ggplot(agg_df, aes(avg_gnipercap))+
  geom_histogram(binwidth = 200)+
  theme_bw()+ylab("Count")
save_plot(file = "../Visualization/Distributions & Densities/Distribution of avg_gnipercap.png", plot = last_plot())

p <- list()

vars <- c("pc1_10_rr_year10", "avg_annual_gni_change", "pneighbor", "pinc_low", 
          "pinc_middle", "pinc_high", "phighref", "phighcasyr0",
          "psomecasyr010", "pinformal_33", "pinformal_50", "punemp_5", "punemp_10", 
          "punemp_15", "pedspend_2", "pedspend_3", "pedspend_4")

for (i in seq_along(vars)) {
  p[[i]] <- ggplot(agg_df, aes_string(vars[i]))+
    geom_histogram(binwidth = 0.05)+geom_vline(mapping = aes(xintercept = 0.5))+
    theme_bw()+ylab("Count")
  save_plot(file = paste0("../Visualization/Distributions & Densities/Distribution of ", vars[i], ".png"), plot = p[[i]])
}

#####
## Bivariate Regressions

## End of Conflict  
ggplot(agg_df, mapping = aes(x = year0, y = pc1_10_rr_year10))+
  geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                       c(-Inf, 0.5, Inf), 
                                       right = FALSE)), 
             alpha = 0.7, 
             size = 3)+
  geom_smooth(method = "lm", color = "grey50", se = FALSE)+
  xlab("Year of Conflict Ending")+
  scale_y_continuous(name = "Proportion of Refugees Returned",
                     limits = c(0, 1))+
  scale_color_manual(name = "",
                     values = c("[-Inf,0.5)" = "#ca0020",
                                "[0.5, Inf)" = "#0571b0"))+
  theme_bw()+ theme(legend.position = "none") 
save_plot(file = "../Visualization/Bivariate Analysis/BR-year0.png", plot = last_plot())

## Refugee Population at the End of Conflict
temp_df <- agg_df[agg_df$ref_year0 >= 10000, ]

ggplot(temp_df, mapping = aes(x = ref_year0, y = pc1_10_rr_year10))+
  geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                       c(-Inf, 0.5, Inf), 
                                       right = FALSE)), 
             alpha = 0.7, 
             size = 3)+
  geom_smooth(method = "lm", color = "grey50", se = FALSE)+
  scale_color_manual(name = "",
                     values = c("[-Inf,0.5)" = "#ca0020",
                                "[0.5, Inf)" = "#0571b0"))+
  theme_bw()+ theme(legend.position = "none")+
  scale_x_continuous(name = "Refugee Population at the End of Conflict",
                     labels = comma,
                     trans = "log10")+
  scale_y_continuous(name = "Proportion of Refugees Returned",
                     limits = c(0, 1))+
  geom_text(aes(label = ifelse({ref_year0 >= 1000000 & pc1_10_rr_year10 >= 0.5},case,"")), 
            hjust = 1.1)
save_plot(file = "../Visualization/Bivariate Analysis/BR-ref_year0.png", plot = last_plot())

## Casualty Years after the End of Conflict
p <- list()

vars <- c("cas25_year", "cas500_year", "cas1k_year")

for (i in seq_along(vars)) {
  p[[i]] <- ggplot(agg_df, mapping = aes_string(x = vars[i], y = "pc1_10_rr_year10"))+
    geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                         c(-Inf, 0.5, Inf), 
                                         right = FALSE)), 
               alpha = 0.7, 
               size = 3)+
    geom_smooth(method = "lm", color = "grey50", se = FALSE)+
    scale_color_manual(name = "",
                       values = c("[-Inf,0.5)" = "#ca0020",
                                  "[0.5, Inf)" = "#0571b0"))+
    theme_bw()+ theme(legend.position = "none")+
    scale_x_continuous(breaks = seq(0, 10, by = 2), minor_breaks = FALSE)+
    scale_y_continuous(name = "Proportion of Refugees Returned",
                       limits = c(0, 1))
  save_plot(file = paste0("../Visualization/Bivariate Analysis/BR-", vars[i], ".png"), plot = p[[i]])  
}

## Length of Conflict  
ggplot(agg_df, mapping = aes(x = length_war, y = pc1_10_rr_year10))+
  geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                       c(-Inf, 0.5, Inf), 
                                       right = FALSE)), 
             alpha = 0.7, 
             size = 3)+
  geom_smooth(method = "lm", color = "grey50", se = FALSE)+
  scale_color_manual(name = "",
                     values = c("[-Inf,0.5)" = "#ca0020",
                                "[0.5, Inf)" = "#0571b0"))+
  theme_bw()+ theme(legend.position = "none")+
  scale_x_continuous(name = "Length of Conflict")+
  scale_y_continuous(name = "Proportion of Refugees Returned",
                     limits = c(0, 1))
save_plot(file = "../Visualization/Bivariate Analysis/BR-length_war.png", plot = last_plot())

## Average GNI Percapita
ggplot(agg_df, mapping = aes(x = avg_gnipercap, y = pc1_10_rr_year10))+
  geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                       c(-Inf, 0.5, Inf), 
                                       right = FALSE)), 
             alpha = 0.7, 
             size = 3)+
  geom_smooth(data = agg_df[agg_df$avg_gnipercap <= 5000,], 
              method = "lm", color = "grey50", se = FALSE)+
  scale_color_manual(name = "",
                     values = c("[-Inf,0.5)" = "#ca0020",
                                "[0.5, Inf)" = "#0571b0"))+
  theme_bw()+ theme(legend.position = "none")+
  scale_x_continuous(name = "Average GNI Per Capita")+
  scale_y_continuous(name = "Proportion of Refugees Returned",
                     limits = c(0, 1))
save_plot(file = "../Visualization/Bivariate Analysis/BR-avg_gnipercap.png", plot = last_plot())

## Annualized GNI Growth Rate

ggplot(agg_df, mapping = aes_string(x = "avg_annual_gni_change", y = "pc1_10_rr_year10"))+
  geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                       c(-Inf, 0.5, Inf), 
                                       right = FALSE)), 
             alpha = 0.7, 
             size = 3)+
  geom_smooth(data = agg_df[agg_df$avg_annual_gni_change <= 0.5, ], method = "lm", color = "grey50", se = FALSE)+
  scale_color_manual(name = "",
                     values = c("[-Inf,0.5)" = "#ca0020",
                                "[0.5, Inf)" = "#0571b0"))+
  theme_bw()+ theme(legend.position = "none")+
  scale_x_continuous(limits = c(-0.2, 1))+
  scale_y_continuous(name = "Proportion of Refugees Returned",
                     limits = c(0, 1))+
  coord_equal()+geom_hline(yintercept = 0.5)+geom_vline(xintercept = 0.5)
save_plot(file = paste0("../Visualization/Bivariate Analysis/BR-", "avg_annual_gni_change", ".png"), plot = last_plot())

p <- list()

vars <- c("pneighbor", "pinc_low", "pinc_middle", "pinc_high", 
          "phighref", "phighcasyr0", "psomecasyr010", "pinformal_33", "pinformal_50", 
          "punemp_5", "punemp_10", "punemp_15", "pedspend_2", "pedspend_3", "pedspend_4")

for (i in seq_along(vars)) {
  p[[i]] <- ggplot(agg_df, mapping = aes_string(x = vars[i], y = "pc1_10_rr_year10"))+
    geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                         c(-Inf, 0.5, Inf), 
                                         right = FALSE)), 
               alpha = 0.7, 
               size = 3)+
    geom_smooth(method = "lm", color = "grey50", se = FALSE)+
    scale_color_manual(name = "",
                       values = c("[-Inf,0.5)" = "#ca0020",
                                  "[0.5, Inf)" = "#0571b0"))+
    theme_bw()+ theme(legend.position = "none")+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(name = "Proportion of Refugees Returned",
                       limits = c(0, 1))+
    coord_equal()+geom_hline(yintercept = 0.5)+geom_vline(xintercept = 0.5)
  save_plot(file = paste0("../Visualization/Bivariate Analysis/BR-", vars[i], ".png"), plot = p[[i]])
}

#####
## T tests

vars <- c("cas25_year", "cas500_year", "cas1k_year", 
          "length_war", 
          "avg_gnipercap", "avg_annual_gni_change",
          "pneighbor",
          "pinc_low", "pinc_middle", "pinc_high", 
          "phighref", "phighcasyr0", "psomecasyr010",
          "pinformal_33", "pinformal_50",
          "punemp_5", "punemp_10", "punemp_15",
          "pedspend_4", "pedspend_3", "pedspend_2")

agg_df$success <- FALSE
agg_df$success[agg_df$pc1_10_rr_year10>= 0.5] <- TRUE

t_results <- list()

for (i in seq_along(vars)) {
  t_results[[i]] <- tidy(t.test(as.formula(paste0(vars[i], " ~ success")), data = agg_df))
}

t_results <- do.call(rbind, t_results)
t_results$vars <- vars
t_results <- t_results[t_results$p.value <= 0.05, ]
t_results <- t_results[, c("vars", "estimate1", "estimate2", "p.value")]
names(t_results) <- c("vars", "estimate_return_faliure", "estimate_return_success", "p.value")

#####
## TriVariate Analysis

home_security <- c("cas25_year",
                   "cas500_year",
                   "cas1k_year")
host_security <- c("phighref",
                   "phighcasyr0",
                   "psomecasyr010")

vars <- expand.grid(home_security, host_security)
vars <- sapply(vars, as.character)

p <- list()

for (i in 1:nrow(vars)) {
  p[[i]] <- ggplot(agg_df, 
                   mapping = aes_string(x = vars[i,2], 
                                        y = vars[i,1],
                                        color = "success"))+
    geom_point(alpha = 0.7, 
               size = 3)+
    geom_smooth(method = "lm", se = FALSE)+
    scale_color_manual(name = "",
                       values = c("FALSE" = "#ca0020",
                                  "TRUE" = "#0571b0"),
                       labels = c("Unsuccessful Return", "Successful Return"))+
    theme_bw()+ theme(legend.position = "none")+
    scale_x_continuous(limits = c(0, 1))+
    scale_y_continuous(limits = c(0, 10))
}

security_plots <- plot_grid(plotlist = p, nrow = 3, byrow = FALSE)

security_legend <- get_legend(
  # create some space to the left of the legend
  p[[1]] + theme(legend.position = "bottom")
)

security_plots <- plot_grid(security_plots, security_legend, nrow = 2, rel_heights = c(1, 0.1))

save_plot(filename = "../Visualization/Trivariate Analysis/Security Trivariate Plot.png", plot = security_plots, base_height = 10)

home_economy <- c("avg_gnipercap",
                  "avg_annual_gni_change")
host_economy <- c("pinc_low",
                  "pinc_middle",
                  "pinc_high")

vars <- expand.grid(home_economy, host_economy)
vars <- sapply(vars, as.character)

p <- list()

for (i in 1:nrow(vars)) {
  p[[i]] <- ggplot(agg_df, 
                   mapping = aes_string(x = vars[i,2], 
                                        y = vars[i,1],
                                        color = "success"))+
    geom_point(alpha = 0.7, 
               size = 3)+
    geom_smooth(method = "lm", se = FALSE)+
    scale_color_manual(name = "",
                       values = c("FALSE" = "#ca0020",
                                  "TRUE" = "#0571b0"),
                       labels = c("Unsuccessful Return", "Successful Return"))+
    theme_bw()+ theme(legend.position = "none")+
    scale_x_continuous(limits = c(0, 1))
}

economy_plots <- plot_grid(plotlist = p, nrow = 3, byrow = TRUE)

economy_legend <- get_legend(
  # create some space to the left of the legend
  p[[1]] + theme(legend.position = "bottom")
)

economy_plots <- plot_grid(economy_plots, economy_legend, nrow = 2, rel_heights = c(1, 0.1))

save_plot(filename = "../Visualization/Trivariate Analysis/Economy Trivariate Plot.png", plot = economy_plots, base_height = 10)


## Get all variables within these categories
pca_vars <- c("pneighbor", "pinc_low", "phighref", "pinformal_50", "punemp_15")

## Create unique combinations of 2
vars <- t(combn(pca_vars, 2))

p <- list()
## Plot Trivariate b/w combination of host vars
for (i in seq_len(nrow(vars))) {
  p[[i]] <- ggplot(agg_df, mapping = aes_string(x = vars[i, 1], y = vars[i, 2]))+
    geom_point(mapping = aes(color = cut(pc1_10_rr_year10, 
                                         c(-Inf, 0.5, Inf), 
                                         right = FALSE)), 
               alpha = 0.7)+
    geom_smooth(mapping = aes(color = cut(pc1_10_rr_year10,
                                          c(-Inf, 0.5, Inf),
                                          right = FALSE)),
                method = "lm", se = FALSE)+
    scale_x_continuous(name = vars[i, 1],
                       limits = c(0, 1))+
    scale_y_continuous(name = vars[i, 2],
                       limits = c(0, 1))+
    scale_color_manual(name = "",
                       values = c("[-Inf,0.5)" = "#ca0020",
                                  "[0.5, Inf)" = "#0571b0"))+
    theme_bw()+ theme(legend.position = "none")+
    coord_equal()+geom_hline(yintercept = 0.5)+geom_vline(xintercept = 0.5)
  save_plot(file = paste0("../Visualization/Trivariate Analysis/TR-", vars[i, 1], "-", vars[i, 2], ".png"), plot = p[[i]])
}

host_vars <- c("pneighbor", "pinc_low", "phighref", "pinformal_50", "punemp_15", "pedspend_2")
home_vars <- c("cas25_year", "cas500_year", "cas1k_year", "length_war", "avg_gnipercap", "avg_annual_gni_change")

vars <- expand.grid(home_vars, host_vars, stringsAsFactors = FALSE)

# p <- list()
# ## Plot Trivarate b/w combination of host vars
# for (i in seq_len(nrow(vars))) {
#   p[[i]] <- ggplot(agg_df, mapping = aes_string(x = vars[i, 1], y = vars[i, 2]))+
#     geom_point(alpha = 0.7, mapping = aes(size = pcrr_size))+
#     geom_smooth(method = "lm", se = TRUE)+
#     scale_x_continuous(name = vars[i, 1])+
#     scale_y_continuous(name = vars[i, 2],
#                        limits = c(0, 1))+
#     scale_size_manual(name = "Proportion of\nRefugees Returned",
#                       breaks = c("Less then 25%",
#                                  "25-50%",
#                                  "More then 50%"),
#                       values = c("Less then 25%" = 3,
#                                  "25-50%" = 6,
#                                  "More then 50%" = 9))+theme_bw()
#   save_plot(file = paste0("../Visualization/Trivariate Analysis/TR-", vars[i, 1], "-", vars[i, 2], ".png"), plot = p[[i]])
# }
# 
# rm(p)

###### Final Output
## Create a categorical variable for size based on proportion of refugees returned
agg_df$pcrr_size2 <- as.character(cut(agg_df$pc1_10_rr_year10, 
                                      breaks = c(-Inf, 0.5, Inf),
                                      labels = c("Less then 50%",
                                                 "More then 50%")))
host_vars <- c("pinc_low", "pinc_middle", "pinc_high")
home_vars <- c("avg_gnipercap", "avg_annual_gni_change")

vars <- expand.grid(home_vars, host_vars, stringsAsFactors = FALSE)

p <- list()
## Plot Trivarate b/w combination of host vars
for (i in seq_len(nrow(vars))) {
  p[[i]] <- ggplot(agg_df, mapping = aes_string(x = vars[i, 1], y = vars[i, 2], size = "pcrr_size2"))+
    geom_point(alpha = 0.7)+
    geom_smooth(method = "lm", se = FALSE)+
    scale_x_continuous(name = vars[i, 1])+
    scale_y_continuous(name = vars[i, 2],
                       limits = c(0, 1))+
    scale_size_manual(name = "Proportion of\nRefugees Returned",
                      breaks = c("Less then 25%",
                                 "25-50%",
                                 "More then 50%"),
                      values = c("Less then 25%" = 3,
                                 "25-50%" = 6,
                                 "More then 50%" = 9))+theme_bw()
  save_plot(file = paste0("../Visualization/Trivariate Analysis/TR-", vars[i, 1], "-", vars[i, 2], ".png"), plot = p[[i]])
}




print("6a")
