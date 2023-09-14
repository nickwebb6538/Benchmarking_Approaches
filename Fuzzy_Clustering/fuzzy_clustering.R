library(tidyverse)
library(vegan)
library(cluster)
library(labdsv)
library(NbClust)
library(stringr)
library(tidyr)
library(ggplot2)


# Read in data
aero <- read.csv("data/indicators/aero.csv")
aero <- dplyr::select(aero, -X)
BLM_indicators <- read.csv("data/indicators/BLM_indicators.csv")
# Table names are offset - shift to the left
names(BLM_indicators) = names(BLM_indicators)[-1]
indicators <- read.csv("data/indicators/indicators.csv")
indicators <- dplyr::select(indicators, -X)
### DATA PREP
# Create reference analysis table with functional group and structure data
names(BLM_indicators)
analysis.table <- dplyr::select(BLM_indicators, 1, 25, 27, 31, 32, 33, 34, 46, 65)
# Join with indicators
analysis.table <- dplyr::left_join(analysis.table, indicators, by = "PrimaryKey")
analysis.table <- na.omit(analysis.table)
# Add row number as lookup index
analysis.table <- dplyr::mutate(analysis.table, ID = rownames(analysis.table))
# Combine gap
analysis.table$LargeGaps <- analysis.table$GapCover_101_200 + analysis.table$GapCover_200_plus
# Create table for ordination and clustering
names(analysis.table)
# cor <- as.data.frame(cor(analysis.table[, -c(1, 17)]))
# First pass
fg.foliar <- dplyr::select(analysis.table, 2, 3, 4, 6, 7, 8, 12, 15, 18)
# Trial run: just indicators
# fg.foliar <- dplyr::select(analysis.table, 12, 13, 15, 16, 18)

# Double check variables
names(fg.foliar)
str(fg.foliar)
# apply standardization
# fg.foliar.s <- vegan::decostand(fg.foliar, method = "hellinger")

# Look at distributions
names(fg.foliar)
ggplot2::ggplot(fg.foliar, aes(BareSoilCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar, aes(LargeGaps)) + geom_histogram()
ggplot2::ggplot(fg.foliar, aes(ScaledGap)) + geom_histogram()
ggplot2::ggplot(fg.foliar, aes(AH_NonNoxPerenForbCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar, aes(AH_NonNoxPerenGrassCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar, aes(AH_NonNoxShrubCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar, aes(AH_NonNoxTreeCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar, aes(AH_NoxCover)) + geom_histogram()

# Post transformation
ggplot2::ggplot(fg.foliar.s, aes(BareSoilCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar.s, aes(LargeGaps)) + geom_histogram()
ggplot2::ggplot(fg.foliar.s, aes(AH_NonNoxPerenForbCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar.s, aes(AH_NonNoxPerenGrassCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar.s, aes(AH_NonNoxShrubCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar.s, aes(AH_NonNoxTreeCover)) + geom_histogram()
ggplot2::ggplot(fg.foliar.s, aes(AH_NoxCover)) + geom_histogram()

### SET SEED
# Set seed for repeatability
set.seed(321)

### ORDINATION
# Make dissimilarity index
fg.foliar.dist <- vegan::vegdist(fg.foliar, method = "gower", binary = FALSE)
# Ordinate (PCoA) on two axes
vegPCA <- cmdscale(fg.foliar.dist, k = 2)
# Plot ordination
plot(vegPCA)
fit <- vegan::envfit(vegPCA, fg.foliar, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "blue")
fit

### FUZZY CLUSTERING
# Test cluster number metrics
# fg.KM.cascade <- cascadeKM(fg.foliar.dist, inf.gr = 3, sup.gr = 20, iter = 100, criterion = "ssi")
# plot(fg.KM.cascade, sortg = TRUE)
# NbClust(fg.foliar, diss = fg.foliar.dist, distance = NULL, min.nc = 3, max.nc = 20, method = "kmeans", index = "all")
# par(mfrow=c(1,1))


# Adjust fuzziness/crispness with membership exponent approaching 2 for fuzzier classification
veg.fanny <- cluster::fanny(fg.foliar.dist, k = 6, memb.exp = 1.15, maxit = 1000, keep.diss = TRUE)
# Display's Dunn's partition coefficient (low coeff = very fuzzy, near 1 = crisp)
veg.fanny$coeff
# Build a dataframe of membership values
fanny.mems <- as.data.frame(veg.fanny$membership)
fanny.mems <- fanny.mems %>%
  dplyr::mutate_if(is.numeric, round, digits = 3)
# Plot clusters in ordination space
colors <- c("aquamarine4", "aquamarine3", "aquamarine1", "pink", "palevioletred2", "palevioletred4")
plot(vegPCA)
stars(veg.fanny$membership, locatio = vegPCA, draw.segm = TRUE, add = TRUE,
      scale = FALSE, len = 0.05,
      col.segments = colors, labels = NULL)
ordihull(vegPCA, veg.fanny$clustering, col = "black")
ordispider(vegPCA, veg.fanny$clustering, col = "gray", label = T)
# Fit vectors over plot
fit <- vegan::envfit(vegPCA, fg.foliar, perm = 999, na.rm = TRUE, choices = c(1, 2, 3))
plot(fit, p.max = 0.05, col = "red")
fit
# Assign plots to clusters by top membership value
names(fanny.mems)
topmems <- fanny.mems %>%
  dplyr::mutate(ID = rownames(fanny.mems)) %>%
  tidyr::gather(Cluster, MemVal, V1:V6) %>%
  dplyr::group_by(ID) %>%
  dplyr::arrange(MemVal) %>%
  dplyr::slice(which.max(MemVal)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Cluster, MemVal)
topmems <- dplyr::left_join(topmems, analysis.table)
# This process relies on the ID column to link plots to clustering results
# QC to make sure ID's are linked properly between topmems and analysis.table
# by crossreferencing clusters, indicator values, and envfit vectors
names(topmems)
clustercheck <- topmems %>%
  dplyr::filter(Cluster == "V2" | Cluster == "V3") %>%
  dplyr::select(PrimaryKey, Cluster, BareSoilCover, AH_NonNoxPerenGrassCover, GapCover_200_plus)
# Summarize indicators within clusters
names(topmems)
topmem.summary <- topmems %>%
  dplyr::select(AH_NonNoxPerenForbCover:LargeGaps, Cluster) %>%
  tidyr::gather(Indicator, MeanCover, AH_NonNoxPerenForbCover:LargeGaps, factor_key = FALSE) %>%
  dplyr::group_by(Cluster, Indicator) %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::summarise_all(funs(mean))
# Sort by highest
topmem.summary <- topmem.summary %>%
  dplyr::group_by(Indicator) %>%
  dplyr::arrange(desc(MeanCover), .by_group = TRUE)
write.csv(topmem.summary, "topmem.summary.csv", row.names = FALSE)

# Separate plots that have high membership values to a cluster from "fuzzy" plots
high.mems <- filter(topmems, MemVal > 0.75)
# What is the percentage of plots with high membership values vs. percentage of fuzzy plots?
(513*100)/678
# 577 are high (85%)



###### CREATE MODAL CONCEPTS
# Rename clusters
high.mems <- high.mems %>%
  dplyr::mutate(ClusterR = ifelse(Cluster == "V6", "C2",
                                  ifelse(Cluster == "V2", "C1",
                          ifelse(Cluster == "V1", "C4",
                          ifelse(Cluster == "V3", "C5",
                          ifelse(Cluster == "V5", "C3",
                          ifelse(Cluster == "V4", "C6", NA))))))) %>%
  dplyr::select(-Cluster, Cluster = ClusterR)

# Summarize minima, maxima, and mean functional group covers
# for each cluster based on plots with high membership value (> 0.5)
# Fuzzy plots will be compared to these modal concepts after they are defined
# Gather means
high.mems <- dplyr::left_join(high.mems, aero)
high.mems <- dplyr::select(high.mems, 1:20, 23)

names(high.mems)
high.means <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:horizontal_flux_total_MD, Cluster) %>%
  gather(Indicator, MeanCover, AH_NonNoxPerenForbCover:horizontal_flux_total_MD, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(mean))
# Gather minima
high.mins <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:horizontal_flux_total_MD, Cluster) %>%
  gather(Indicator, MinCover, AH_NonNoxPerenForbCover:horizontal_flux_total_MD, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(min))
# Gather maxima
high.maxs <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:horizontal_flux_total_MD, Cluster) %>%
  gather(Indicator, MaxCover, AH_NonNoxPerenForbCover:horizontal_flux_total_MD, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(max))
# Median
high.meds <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:horizontal_flux_total_MD, Cluster) %>%
  gather(Indicator, MedCover, AH_NonNoxPerenForbCover:horizontal_flux_total_MD, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  select_if(is.numeric) %>%
  summarise_all(funs(median))
# IQR
high.25 <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:horizontal_flux_total_MD, Cluster) %>%
  gather(Indicator, P25, AH_NonNoxPerenForbCover:horizontal_flux_total_MD, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  summarize(q1 = quantile(P25, 0.25))
high.75 <- high.mems %>%
  dplyr::select(AH_NonNoxPerenForbCover:horizontal_flux_total_MD, Cluster) %>%
  gather(Indicator, P75, AH_NonNoxPerenForbCover:horizontal_flux_total_MD, factor_key = FALSE) %>%
  group_by(Cluster, Indicator) %>%
  summarize(q3 = quantile(P75, 0.75))

# Join summary tables
high.sums <- high.means %>%
  inner_join(high.mins) %>%
  inner_join(high.maxs) %>%
  inner_join(high.meds) %>%
  inner_join(high.25) %>%
  inner_join(high.75) %>%
  mutate_if(is.numeric, round, digits = 2)


# Save csvs
write.csv(high.mems, "high.mems.csv", row.names = FALSE)
write.csv(high.sums, "high.sums.csv", row.names = FALSE)
write.csv(topmems, "topmems.csv", row.names = FALSE)



# Review photos
c1 <- dplyr::filter(high.mems, Cluster == "C2")
c1pks <- subset(BLM_indicators, BLM_indicators$PrimaryKey %in% c1$PrimaryKey)
c1pks <- dplyr::select(c1pks, PrimaryKey, EcologicalSiteId)
c1 <- dplyr::left_join(c1, c1pks)
c1pics <- dplyr::filter(c1, EcologicalSiteId == "R042XB014NM" |
                           EcologicalSiteId == "R042XB012NM" |
                          EcologicalSiteId == "R042XB010NM" |
                          EcologicalSiteId == "R042XB023NM")

