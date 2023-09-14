# Quantile approach to setting indicator benchmarks
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

# Read in data
aero <- read.csv("data/indicators/aero.csv")
aero <- dplyr::select(aero, -X)
BLM_indicators <- read.csv("data/indicators/BLM_indicators.csv")
# Table names are offset - shift to the left
names(BLM_indicators) = names(BLM_indicators)[-1]
indicators <- read.csv("data/indicators/indicators.csv")
indicators <- dplyr::select(indicators, -X)
### DATA PREP
indicators$LargeGaps <- indicators$GapCover_101_200 + indicators$GapCover_200_plus

# Make historgram to look at shape of distribution
ggplot2::ggplot(indicators, aes(BareSoilCover)) + geom_histogram()

# Find quantiles
quantile(indicators$BareSoilCover, 0.75)
# 48% is the 75th quantile
# 75% of plots have less than or equal to 48% bare ground
quantile(indicators$BareSoilCover, c(0.25, 0.5, 0.75))
quantile(indicators$LargeGaps, c(0.25, 0.5, 0.75))
quantile(indicators$TotalFoliarCover, c(0.25, 0.5, 0.75))


# Plot indicator boxplots
# Make tall table
names(indicators)
indicatorstall <- tidyr::gather(indicators, key = "Indicator", value = "Value", 2:9)
unique(indicatorstall$Indicator)
inds.tall.100 <- dplyr::filter(indicatorstall, Indicator == "GapCover_101_200" |
                                 Indicator == "GapCover_200_plus" |
                                 Indicator == "BareSoilCover" |
                                 Indicator == "TotalFoliarCover" |
                                 Indicator == "LargeGaps")

meanmaxheight <- dplyr::filter(indicatorstall, Indicator == "MeanMaxHeight")
meangap <- dplyr::filter(indicatorstall, Indicator == "MeanGap")
scaledgap <- dplyr::filter(indicatorstall, Indicator == "ScaledGap")

# Set palette
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal(n = 8, name = "Set2")

indbp.100 <- ggplot2::ggplot(inds.tall.100, aes(x = Indicator, y = Value, fill = Indicator)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3",
                                   name = "Indicator") +
  xlab("Indicator") + ylab("Percent") + ggtitle("Distributions of indicators MLRA 42") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red", fill = "red")

indbp.100

# For scaled indicators
RColorBrewer::brewer.pal(n = 3, name = "Dark2")
mmhplot <- ggplot(meanmaxheight, aes(x = Indicator, y = Value, fill = Indicator)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1B9E77")) +
  theme(legend.position = "none")



mgplot <- ggplot(meangap, aes(x = Indicator, y = Value, fill = Indicator)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#D95F02")) +
  theme(legend.position = "none")



sgplot <- ggplot(scaledgap, aes(x = Indicator, y = Value, fill = Indicator)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#7570B3")) +
  theme(legend.position = "none") +
  scale_y_log10()

scaledgrid <- cowplot::plot_grid(mmhplot, mgplot, sgplot)
scaledgrid


# High mem summary table
names(indicators)
sumtable <- indicatorstall %>%
  dplyr::group_by(Indicator) %>%
  dplyr::summarise()
# IQR
sumtableIQR<- gtsummary::tbl_summary(indicatorstall, by = Indicator, digits = everything() ~ 1)
sumtableIQR <- gtsummary::modify_header(sumtableIQR, label = "**Indicator**")
sumtableIQR <- gtsummary::modify_spanning_header(sumtableIQR, all_stat_cols() ~ "**Cluster**")

sumtableIQR
gt::gtsave(as_gt(sumtableIQR), path = "fuzzy clustering/Figures", filename = "IQR_table.png")
