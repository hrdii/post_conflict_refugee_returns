##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-02-11 for Post-Conflict Refugee Returns Project
## Description: Dimension Reduction using PCA

##### environment set up
rm(list = ls())
options(stringsAsFactors = FALSE)

## Load Libraries
library(data.table)
library(corrplot)
library(ggplot2)
library(cowplot)

## Load Conflict Cases
load("../Intermediate/Conflict case with refugee origin and host state information.RData")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")

## Load Shortlist of Refugee Sources
load("../Intermediate/Shortlist of Major Refugee Sources.Rdata")

row.names(agg_df) <- agg_df$case

vars <- c("cas25_year", "cas500_year", "cas1k_year",
          "length_war",
          "avg_gnipercap", "avg_annual_gni_change",
          "pneighbor",
          "pinc_low", "pinc_middle", "pinc_high",
          "phighref", "phighcasyr0", "psomecasyr010",
          "pinformal_33", "pinformal_50",
          "punemp_5", "punemp_10", "punemp_15")
x <- agg_df[, vars]
res <- cor(x, use = "na.or.complete")
res[abs(res) < 0.4] <- NA
corrplot(res, method = "square", type = "upper", order = "original", 
         tl.col = "black", na.label = " ")

host_inc <- c("pinc_low", "pinc_high", "pinc_middle")
host_insecure <- c("phighref", "phighcasyr0", "psomecasyr010")
host_informal <- c("pinformal_33", "pinformal_50")
host_unemp <- c("punemp_5", "punemp_10", "punemp_15")

host_vars <- expand.grid(host_inc, host_insecure, host_informal, host_unemp, stringsAsFactors = FALSE)
host_vars$Var6 <- "pneighbor"

pca_result <- list()

for (i in seq_len(nrow(host_vars))) {
  vars <- unlist(host_vars[i, ])
  x <- agg_df[, vars]
  df_pca <- prcomp(formula = as.formula(paste0("~", paste(vars, collapse = "+"))),
                   data = x, 
                   na.action = na.omit)
  temp <- t(summary(df_pca)[["importance"]])
  temp <- data.frame(cbind(rownames(temp), temp), stringsAsFactors = FALSE)
  temp[, 2:4] <- lapply(temp[, 2:4], FUN = as.numeric)
  pca_result[[i]] <- c(vars,
                       temp[1,3],
                       temp[1,3] / temp[2,3])
}

pca_result <- do.call(rbind.data.frame, pca_result)

names(pca_result) <- c(paste0("V", seq(from = 1, 
                                       to = {ncol(pca_result) - 2},
                                       by = 1)),
                       "prop_var", "pc1_pc2_ratio")
pca_result[, c("prop_var", "pc1_pc2_ratio")] <- lapply(pca_result[, c("prop_var", "pc1_pc2_ratio")], as.numeric)

summary(pca_result)

pca_result <- pca_result[pca_result$prop_var > 0.5,]

pca_result <- pca_result[order(pca_result$prop_var, pca_result$pc1_pc2_ratio, decreasing = TRUE),]

vars <- unlist(pca_result[1, names(pca_result)[1:{ncol(pca_result) - 2}]])

x <- agg_df[, vars]
df_pca <- prcomp(formula = as.formula(paste0("~", paste(vars, collapse = "+"))),
                 data = x, 
                 na.action = na.omit)

temp <- t(summary(df_pca)[["importance"]])
temp <- data.frame(cbind(rownames(temp), temp), stringsAsFactors = FALSE)
temp[, 2:4] <- lapply(temp[, 2:4], FUN = as.numeric)
ggplot(temp, aes(x = V1, y = Proportion.of.Variance))+
  geom_bar(stat = "Identity")+scale_y_continuous(limits = c(0,1))

add_comp <- data.frame(cbind(rownames(df_pca$x), df_pca$x[, 1:2]))
add_comp[, 2:3] <- lapply(add_comp[, 2:3], FUN = as.numeric)

ggplot(add_comp)+
  geom_point(mapping = aes(x = PC2, y = PC1))+
  coord_equal()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)

## Looking at Component loadings on different variables
df_pca$rotation[,1]
# pinc_low     phighref     pinformal_50 punemp_15    pneighbor 
# 0.6873931    0.4601889    0.1820867    0.1340893    0.5143747 

## PC1 loads strongly positively to pneighbor, pinc_low, and phighref  
## PC1 can be interpreted as refugees in low-income host countries that are 
## neighbors and have high refugee population

df_pca$rotation[,2]
# pinc_low    phighref      pinformal_50 punemp_15     pneighbor 
# 0.47507081  -0.81536785   0.27429096   -0.17966914   0.04434476 

## Adding Principal Components to the data frame
agg_df <- merge(x = agg_df, y = add_comp, by.x = "case", by.y = "V1", all.x = TRUE)

## PC1  
ggplot(agg_df, mapping = aes(x = PC1, y = pc1_10_rr_year10))+
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
  scale_x_continuous(name = "PC1")+
  scale_y_continuous(name = "Proportion of Refugees Returned",
                     limits = c(0, 1))+
  coord_equal()+geom_hline(yintercept = 0.5)
save_plot(plot = last_plot(), filename = "../Visualization/Bivariate Analysis/BR-PC1.png")

## PC2  
ggplot(agg_df, mapping = aes(x = PC2, y = pc1_10_rr_year10))+
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
  scale_x_continuous(name = "PC2")+
  scale_y_continuous(name = "Proportion of Refugees Returned",
                     limits = c(0, 1))+
  coord_equal()+geom_hline(yintercept = 0.5)
save_plot(plot = last_plot(), filename = "../Visualization/Bivariate Analysis/BR-PC2.png")

## Save File
write.csv(agg_df, file = paste0("../Output/Updated ", Sys.Date(), " merged db with PCA.csv"))

print("6b")
