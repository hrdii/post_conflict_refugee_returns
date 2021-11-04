##########----------##########----------##########----------##########----------

##########---------- HEADER

##### meta-information
## Author: Hardika Dayalani(dayalani@rand.org)
## Creation: 2020-10-01 for Post-Conflict Refugee Returns Project
## Description: Host Security Index Using PCA

##### environment set up
rm(list = ls())
options(stringsAsFactors = FALSE)

## Load Libraries
library(data.table)

## Load Conflict Cases
load("../Intermediate/Conflict case with refugee origin and host state information.RData")
## Load Refugee Information
load("../Intermediate/Refugee Population.RData")

## Load Shortlist of Refugee Sources
load("../Intermediate/Shortlist of Major Refugee Sources.Rdata")

row.names(agg_df) <- agg_df$case

vars <- c("phighref", "phighcasyr0", "psomecasyr010")
x <- agg_df[, vars]

pca <- prcomp(x, center = TRUE, scale. = TRUE)

temp <- t(summary(pca)[["importance"]])
temp <- data.frame(cbind(rownames(temp), temp), stringsAsFactors = FALSE)
temp[, 2:4] <- lapply(temp[, 2:4], FUN = as.numeric)

prop_var <- temp[1,3] ## Proportion of Variance explained by PC1 > 0.5
pc1_pc2_ratio <- temp[1,3] / temp[2,3] ## Ratio of PoV of PC1/PC2> 3

## Examining PC1
pca$rotation[,1]
# phighref   phighcasyr0 psomecasyr010 
# 0.5783206     0.5310913     0.6192636
pca$rotation[,2]
# phighref   phighcasyr0 psomecasyr010 
# -0.5713437     0.8055059    -0.1572470 

add_comp <- data.frame(cbind(rownames(pca$x), pca$x[, 1:2]))
add_comp[, 2:3] <- lapply(add_comp[, 2:3], FUN = as.numeric)
names(add_comp) <- c("case", "host_insecurity_index", "host_casualty_index")

## Adding Principal Components to the data frame
agg_df <- merge(x = agg_df, y = add_comp, by = "case", all.x = TRUE)

agg_df$success <- FALSE
agg_df$success[agg_df$pc1_10_rr_year10>= 0.5] <- TRUE

t.test(host_insecurity_index ~ success, data = agg_df)

mod1 <- lm( ~ host_insecurity_index, data = agg_df)
summary(mod1)

mod2 <- lm(avg_annual_gni_change ~ pinc_low, data = agg_df[agg_df$success,])
summary(mod2)

mod3 <- lm(avg_annual_gni_change ~ pinc_low, data = agg_df[!agg_df$success,])
summary(mod3)

mod4 <- lm(pc1_10_rr_year10 ~ avg_annual_gni_change + pinc_low, data = agg_df)
summary(mod4)

vars <- names(agg_df)[107:127]

p <- list()
for (i in 1:length(vars)) {
  mod <- lm(as.formula(paste0("pc1_10_rr_year10 ~", vars[i])),
            data = agg_df)
  f <- summary(mod)$fstatistic
  p_value <- pf(f[1],f[2],f[3],lower.tail=F)
  p[[i]] <- c(mod$coefficients[2], p_value)
  
}

p <- do.call(rbind, p)

p <- cbind(vars, p)

vars <- vars[c(F, F, T, T, T, T, T, T, T, T, T, T, T, F, T, F, T, F, F, F, F)]
form <- paste0(vars, collapse = "+")

mod5 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", form)), data = agg_df)
summary(mod5)

mod6 <- lm(pc1_10_rr_year10 ~ pneighbor + pinc_low, data = agg_df)
summary(mod6)

p <- data.frame(p)
p[,2:3] <- sapply(p[,2:3], as.numeric)

vars <- p[p$value<= 0.05, "vars"]

vars <- vars[c(1:5, 7)]

vars <- c(vars, "punemp_10")

form <- paste0(vars, collapse = "+")

mod7 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", form)), data = agg_df)
summary(mod7)

form <- paste0(vars, collapse = "+")

mod7 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", form)), data = agg_df)
summary(mod7)

cor_tab <- cor(agg_df[, vars])

row.names(agg_df) <- agg_df$case
x <- agg_df[complete.cases(agg_df), vars]

pca <- prcomp(x, center = TRUE, scale. = TRUE)

temp <- t(summary(pca)[["importance"]])
temp <- data.frame(cbind(rownames(temp), temp), stringsAsFactors = FALSE)
temp[, 2:4] <- lapply(temp[, 2:4], FUN = as.numeric)

prop_var <- temp[1,3] ## Proportion of Variance explained by PC1 > 0.5
pc1_pc2_ratio <- temp[1,3] / temp[2,3] ## Ratio of PoV of PC1/PC2> 3

## Examining PC1
pca$rotation[,1]
# cas25_year avg_gnipercap     pneighbor 
# 0.18504706    0.44316264   -0.46992904 
# pinc_low     pinc_high  pinformal_50    punemp_10 
# -0.47327813    0.49779524   -0.27593783 -0.02443713

pca$rotation[,2]
# cas25_year avg_gnipercap     pneighbor 
# -0.424286135  -0.011563545  -0.171205438 
# pinc_low     pinc_high  pinformal_50      punemp_10 
# -0.155940349   0.005674557   0.337682631  -0.807562447

vars <- c("pneighbor", "pinc_low", "pinc_high", "pinformal_50") 

x <- agg_df[, vars]

pca <- prcomp(x, center = TRUE, scale. = TRUE)

temp <- t(summary(pca)[["importance"]])
temp <- data.frame(cbind(rownames(temp), temp), stringsAsFactors = FALSE)
temp[, 2:4] <- lapply(temp[, 2:4], FUN = as.numeric)

prop_var <- temp[1,3] ## Proportion of Variance explained by PC1 > 0.5
pc1_pc2_ratio <- temp[1,3] / temp[2,3] ## Ratio of PoV of PC1/PC2> 3

## Examining PC1
pca$rotation[,1]

# pneighbor     pinc_low    pinc_high pinformal_50 
# 0.5440011    0.5279862   -0.5626683    0.3296934 

## Cases where host countries are neighboring, low-income and
  ## with significant in informal economies

pca$rotation[,2]

# pneighbor     pinc_low    pinc_high pinformal_50 
# -0.28890252  -0.05675297   0.21327861   0.93157215 

add_comp <- data.frame(cbind(rownames(pca$x), pca$x[, 1:2]))
add_comp[, 2:3] <- lapply(add_comp[, 2:3], FUN = as.numeric)
names(add_comp) <- c("case", "low_inc_neighbor_index", "informality_index")

## Adding Principal Components to the data frame
agg_df <- merge(x = agg_df, y = add_comp, by = "case", all.x = TRUE)

mod8 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", "low_inc_neighbor_index")), data = agg_df)
summary(mod8)

mod9 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", "informality_index")), data = agg_df)
summary(mod9)

form <- paste0(c("cas25_year", "avg_gnipercap", "punemp_10", "low_inc_neighbor_index"), collapse = "+")  

mod10 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", form)), data = agg_df)
summary(mod10)

form <- paste0(c("cas25_year", "avg_gnipercap","low_inc_neighbor_index"), collapse = "+")  

mod11 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", form)), data = agg_df)
summary(mod11)

form <- paste0(c("cas25_year", "low_inc_neighbor_index"), collapse = "+")  

mod12 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", form)), data = agg_df)
summary(mod12)

mod14 <- lm(as.formula(paste0("pc1_10_rr_year10 ~ ", "cas500_year")), data = agg_df)
summary(mod14)



library(ggplot2)
ggplot(agg_df)+
  geom_point(mapping = aes(x = pneighbor, y = pinc_low))+
  coord_equal()
