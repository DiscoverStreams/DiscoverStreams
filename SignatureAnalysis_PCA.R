# data manipulation libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(reshape2)
# database connection/retrieval libraries
library(readr)
# data analysis/statistical libraries





#############################################################
### PCA ###
## SET watershed dataframe to work with based on time period
ws_040500MI <- ws_040500MI_hd
ws_110300KS <- ws_110300KS_hd
ws_180102CA <- ws_180102CA_hd

ws_040500MI <- ws_040500MI_long
ws_110300KS <- ws_110300KS_long
ws_180102CA <- ws_180102CA_long

ws_040500MI <- ws_040500MI_ep
ws_110300KS <- ws_110300KS_ep
ws_180102CA <- ws_180102CA_ep

## ISOLATE station names, metrics labels, station IDs, and metrics for PCA & plotting options
pca_stations <- ws_040500MI$station_nm
pca_metrics <- colnames(ws_040500MI[ , 24:29])
pca_ids <- ws_040500MI$id
pca_select <- ws_040500MI[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")]

pca_stations <- ws_110300KS$station_nm
pca_metrics <- colnames(ws_110300KS[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")])
pca_ids <- ws_110300KS$id
pca_select <- ws_110300KS[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")]

pca_stations <- ws_180102CA$station_nm
pca_metrics <- colnames(ws_180102CA[ , 24:29])
pca_ids <- ws_180102CA$id
pca_select <- ws_180102CAS[ , c("Q10", "MeanQ", "Q50", "Baseflow", "MAM7", "Q90", "Q95")]


## REFORMAT columns from lists to numerics for compatibility with princomp function
pca_select$Q90 <- as.numeric(pca_select$Q90)
pca_select$MeanQ <- as.numeric(pca_select$MeanQ)
pca_select$Baseflow <- as.numeric(pca_select$Baseflow)
pca_select$MAM7 <- as.numeric(pca_select$MAM7)

## COMPUTE PCA on metrics per station for each watershed time period 
pca_040500MI_hd <- princomp(pca_select, cor = TRUE)
pca_040500MI_long <- princomp(pca_select, cor = TRUE)
pca_040500MI_ep <- princomp(pca_select, cor = TRUE)

pca_110300KS_hd <- princomp(pca_select, cor = TRUE)
pca_110300KS_long <- princomp(pca_select, cor = TRUE)
pca_110300KS_ep <- princomp(pca_select, cor = TRUE)

pca_180102CA_hd <- princomp(pca_select, cor = TRUE)
pca_180102CA_long <- princomp(pca_select, cor = TRUE)
pca_180102CA_ep <- princomp(pca_select, cor = TRUE)

## SUMMARY of PCA results
summary(pca_040500MI_hd)
loadings(pca_040500MI_hd)
str(pca_040500MI_hd)

summary(pca_110300KS_hd)
loadings(pca_110300KS_hd)
str(pca_110300KS_hd)

summary(pca_180102CA_hd)
loadings(pca_180102CA_hd)
str(pca_180102CA_hd)

summary(pca_040500MI_long)
loadings(pca_040500MI_long)
str(pca_040500MI_long)

summary(pca_110300KS_long)
loadings(pca_110300KS_long)
str(pca_110300KS_long)

summary(pca_180102CA_long)
loadings(pca_180102CA_long)
str(pca_180102CA_long)

summary(pca_040500MI_ep)
loadings(pca_040500MI_ep)
str(pca_040500MI_ep)

summary(pca_110300KS_ep)
loadings(pca_110300KS_ep)
str(pca_110300KS_ep)

summary(pca_180102CA_ep)
loadings(pca_180102CA_ep)
str(pca_180102CA_ep)


## SELECT PCA results for plotting
pca_results <- pca_040500MI_hd
pca_results <- pca_110300KS_hd
pca_results <- pca_180102CA_hd

pca_results <- pca_040500MI_long
pca_results <- pca_110300KS_long
pca_results <- pca_180102CA_long

pca_results <- pca_040500MI_ep
pca_results <- pca_110300KS_ep
pca_results <- pca_180102CA_ep

## SCREE PLOT of PCA components
screeplot(pca_results, type = "lines", main = "Screeplot of PCA Variances for Streamflow Metrics")

## SCREE PLOT with Variance Explained on y-axis instead of # of variances
## https://www.statology.org/principal-components-analysis-in-r/
pca_var_explained <- pca_results$sdev^2 / sum(pca_results$sdev^2)
qplot(x = c(1:7), pca_var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot of PCA Variances for Streamflow Metrics") +
  scale_x_continuous(breaks = 1:6) +
  ylim(0, 1)

## PLOT PCA scores
autoplot(pca_results, data = pca_select, colour = as.factors(pca_stations))

## MULTIPLY by -1 to adjust for R package calculations
pca_results$loadings <- pca_results$loadings
## PLOT PCA loadings
plot(pca_results$loadings, pch = 16, col = c("#9451cc", "#06ccd6", "#d19d43", "#0939a5", "#259f3d", "#a11437", "#12173e"), main = "PCA  Loadings for Streamflow Metrics in Middle Arkansas River Watershed", xlim = c(-1, 1), ylim = c(-1, 1))
segments(c(-1, 0), c(0, -1), c(1, 0), c(0, 1))
text(pca_results$loadings,
     labels = pca_metrics,
     cex = 1, pos = 3)
legend("topright", legend = pca_metrics, pch = 16, col = c("#9451cc", "#06ccd6", "#d19d43", "#0939a5", "#259f3d", "#a11437", "#12173e"))


## PLOT PCA biplot of scores and loadings
biplot(pca_results, xlabs = pca_ids)



