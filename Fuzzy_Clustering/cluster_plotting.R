# Plotting fuzzy clusters and Q
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(webshot2)
library(gt)
library(RColorBrewer)
library(cowplot)
library(grid)
library(gridExtra)

# Add itaclis to Q in all plots
# bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")")



# Read in cluster assigned data
high.mems <- read.csv("fuzzy clustering/high.mems.csv")


# Plot indicators, Q, clusters, and benchmarks
# Set color palette
colors <- c("#88CCEE", "#CC6677",
            "#DDCC77", "#117733",
            "#332288", "#AA4499")


# Bare soil
baseplot <- ggplot() +
  geom_point(data = high.mems, aes(x = BareSoilCover,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 1) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = BareSoilCover, y = horizontal_flux_total_MD),
              color = "darkgray", se = F, linewidth = 0.5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        text = element_text(size = 16)) +
  ylab(bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")")) +
  scale_x_continuous(name = "Bare Soil (%)")

baseplot

# Cluster IQRs
bs_allcluster_plot <- baseplot + geom_vline(data = high.mems %>%
                                              dplyr::filter(Cluster == "C1"),
                                            aes(xintercept = quantile(BareSoilCover,
                                                                      probs = 0.50)),
                                            color = "#88CCEE",
                                            size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.50)),
             color = "#CC6677",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "#CC6677", size = 0.5, lty = 2) +
  theme(legend.position = "none") +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.50)),
             color = "#DDCC77",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.25)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(BareSoilCover,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2)
bs_allcluster_plot

ggsave("fuzzy clustering/Figures/bs_allclusters.png", dpi = 300, height = 4, width = 6, unit = "in")


# Total Foliar
baseplot <- ggplot() +
  geom_point(data = high.mems, aes(x = TotalFoliarCover,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 1) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = TotalFoliarCover, y = horizontal_flux_total_MD),
              color = "darkgray", se = F, linewidth = 0.5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        text = element_text(size = 16)) +
  ylab(bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")")) +
  scale_x_continuous(name = "Total Foliar Cover (%)")

baseplot

# Cluster IQRs
tf_allcluster_plot <- baseplot + geom_vline(data = high.mems %>%
                                              dplyr::filter(Cluster == "C1"),
                                            aes(xintercept = quantile(TotalFoliarCover,
                                                                      probs = 0.50)),
                                            color = "#88CCEE",
                                            size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.25)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.75)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.50)),
             color = "#CC6677",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.25)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.75)),
             color = "#CC6677", size = 0.5, lty = 2) +
  theme(legend.position = "none") +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.50)),
             color = "#DDCC77",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.25)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(TotalFoliarCover,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2)
tf_allcluster_plot

ggsave("fuzzy clustering/Figures/tf_allclusters.png", dpi = 300, height = 4, width = 6, unit = "in")


# Large Gaps
baseplot <- ggplot() +
  geom_point(data = high.mems, aes(x = LargeGaps,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 1) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = LargeGaps, y = horizontal_flux_total_MD),
              color = "darkgray", se = F, linewidth = 0.5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        text = element_text(size = 16)) +
  ylab(bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")")) +
  scale_x_continuous(name = "Gap Cover 100 cm + (%)")

baseplot

# Cluster IQRs
lg_allcluster_plot <- baseplot + geom_vline(data = high.mems %>%
                                              dplyr::filter(Cluster == "C1"),
                                            aes(xintercept = quantile(LargeGaps,
                                                                      probs = 0.50)),
                                            color = "#88CCEE",
                                            size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.50)),
             color = "#CC6677",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "#CC6677", size = 0.5, lty = 2) +
  theme(legend.position = "none") +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.50)),
             color = "#DDCC77",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.25)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(LargeGaps,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2)
lg_allcluster_plot

ggsave("fuzzy clustering/Figures/lg_allclusters.png", dpi = 300, height = 4, width = 6, unit = "in")




# Scaled gap
baseplot <- ggplot() +
  geom_point(data = high.mems, aes(x = ScaledGap,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 1) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  geom_smooth(data = high.mems, aes(x = ScaledGap, y = horizontal_flux_total_MD),
              color = "darkgray", se = F, linewidth = 0.5) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        text = element_text(size = 16)) +
  ylab(bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")")) +
  scale_x_continuous(name = "Scaled Gap", trans = "pseudo_log")

baseplot

# Cluster IQRs
sg_allcluster_plot <- baseplot + geom_vline(data = high.mems %>%
                                              dplyr::filter(Cluster == "C1"),
                                            aes(xintercept = quantile(ScaledGap,
                                                                      probs = 0.50)),
                                            color = "#88CCEE",
                                            size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C1"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#88CCEE", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#CC6677",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#CC6677", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C2"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#CC6677", size = 0.5, lty = 2) +
  theme(legend.position = "none") +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.50)),
             color = "#DDCC77",
             size = 0.5) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.25)),
             color = "#DDCC77", size = 0.5, lty = 2) +
  geom_vline(data = high.mems %>%
               dplyr::filter(Cluster == "C3"),
             aes(xintercept = quantile(ScaledGap,
                                       probs = 0.75)),
             color = "#DDCC77", size = 0.5, lty = 2)
sg_allcluster_plot

ggsave("fuzzy clustering/Figures/sg_allclusters.png", dpi = 300, height = 4, width = 6, unit = "in")


# Legend
legplot <- ggplot() +
  geom_point(data = high.mems, aes(x = ScaledGap,
                                   y = horizontal_flux_total_MD,
                                   color = Cluster, fill = Cluster),
             size = 1.5) +
  scale_fill_manual(values = colors, name = "Cluster") +
  scale_color_manual(values = colors, name = "Cluster") +
  theme_bw()
legplot

legend <- cowplot::get_legend(legplot)

grid.newpage()
grid.draw(legend)


ggsave("fuzzy clustering/Figures/legend.png", dpi = 300, height = 4, width = 6, unit = "in")








# High membership plot summary table
table(high.mems$Cluster)
names(high.mems)
sumtable <- dplyr::select(high.mems, Cluster, AH_NonNoxPerenForbCover, AH_NonNoxPerenGrassCover,
                          AH_NonNoxShrubCover,
                          BareSoilCover, LargeGaps, TotalFoliarCover, ScaledGap, horizontal_flux_total_MD)
sumtable <- dplyr::rename(sumtable, "Perennial Forbs (%)" = AH_NonNoxPerenForbCover, "Perennial Grasses (%)" = AH_NonNoxPerenGrassCover,
                          "Shrubs (%)" = AH_NonNoxShrubCover, "Bare Soil (%)" = BareSoilCover,
                          "Total Foliar Cover (%)" = TotalFoliarCover, "Gaps > 100 cm (%)" = LargeGaps,
                          "Scaled Gap (cm)" = ScaledGap,
                          "Q" = horizontal_flux_total_MD)
sumtable <- dplyr::ungroup(sumtable)
sumtable <- sumtable %>%
  dplyr::mutate(across(where(is.numeric), ~ round(., 1)))
# IQR
sumtableIQR<- gtsummary::tbl_summary(sumtable, by = Cluster, digits = everything() ~ 1,
                                     label = Q ~ "Q (g m<sup>-1</sup> d<sup>-1</sup>)")
sumtableIQR <- gtsummary::modify_header(sumtableIQR, label = "**Indicator**")
sumtableIQR <- gtsummary::modify_spanning_header(sumtableIQR, all_stat_cols() ~ "**Cluster**")

sumtableIQR <- sumtableIQR %>%
  gtsummary::as_gt() %>%
  gt::fmt_markdown(columns = vars(label))

sumtableIQR


gt::gtsave(sumtableIQR, path = "fuzzy clustering/Figures", filename = "IQR_table.png")

# Save as editable format
sumtableIQR %>%
  gt::gtsave(., "fuzzy clustering/Figures/ClusterTable_doc_AH.docx")

