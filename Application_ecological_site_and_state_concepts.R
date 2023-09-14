#### Wind erosion benchmarks - Application of ecological site and state concepts (Manuscript Section 4.2.2, Supplement Section 2.2) ####
## Jeremy W. Schallner, schalln@nmsu.edu ##

## Load Required Packages
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(grid)
library(gridExtra)

#### Data loading and processing ####

## Set working directory to location of the files required for this script (Alter any file paths as needed.)

## Data files required for script: "indicators_2022-10-22.csv", "aero.csv", "BLM_indicators.csv", "sandy_state_all.csv", "EDIT_public_ecological_site_list.csv"

setwd("C:/Users/schalln/OneDrive - New Mexico State University/Desktop/WindErosionBenchmarks-main/WindErosionBenchmarks-main")

## Import indicator data
indicators<-read.csv("data/indicators/indicators_2022-10-22.csv")
aero<-read.csv("data/indicators/aero.csv")
BLM_ind<-read.csv("data/indicators/BLM_indicators.csv")

## Fix column names in BLM data set
names(BLM_ind)[1:(ncol(BLM_ind)-1)]<-names(BLM_ind)[2:ncol(BLM_ind)]
names(BLM_ind)[156]<-"Shape2"

## Import ecological state assignments
sandy_state_all <- read.csv("data/indicators/sandy_state_all.csv")

## Fix EcologicalSiteID for LMF plots
EDIT <- read.csv("data/indicators/EDIT_public_ecological_site_list.csv")
EDIT[["EcoSiteId_Stripped"]] <- gsub(EDIT[["new_es_symbol"]],
                                     pattern = "^[RF]", replacement = "")
#Check to see if unique
ecosite_lut <- unique(EDIT[,c("new_es_symbol" , "EcoSiteId_Stripped")])
any(table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1)
trouble_ids <- names(table(ecosite_lut[["EcoSiteId_Stripped"]]))[table(ecosite_lut[["EcoSiteId_Stripped"]]) > 1]

#Drop the repeat ids
ecosite_lut_drop_bad <- ecosite_lut %>% filter(!EcoSiteId_Stripped == trouble_ids)

#Add a new field called EcologicalSiteId that has the dropped R and F

EcoSites_Update <- ecosite_lut_drop_bad %>% mutate(EcologicalSiteId = EcoSiteId_Stripped)

#Merge the dataframe with the full EcologicalSiteId and dropped R/F Id with the LMF
BLM_ind_LMF<-BLM_ind %>% filter(ProjectName=="LMF")
BLM_ind_LMFEcoSite <- merge(BLM_ind_LMF , EcoSites_Update, by = "EcologicalSiteId")

#Drop the EcologicalSiteId value that we added earlier
BLM_ind_LMFEcoSite <- BLM_ind_LMFEcoSite %>% dplyr::select(-EcologicalSiteId)

#Rename Ecological Site Id to the full Ecological Site Id code (= new_es_symbol)
BLM_ind_LMFEcoSite <- BLM_ind_LMFEcoSite %>% dplyr::rename(EcologicalSiteId = new_es_symbol)
BLM_ind_LMFEcoSite <- BLM_ind_LMFEcoSite %>% dplyr::select(-EcoSiteId_Stripped)

PK_LMF<-BLM_ind_LMF$PrimaryKey
PK_LMF_Eco<-BLM_ind_LMFEcoSite$PrimaryKey
droppedPKs<-setdiff(PK_LMF,PK_LMF_Eco)

## Check dropped IDs (if needed/desired) ##

#view(BLM_ind_LMF %>% filter(PrimaryKey %in% droppedPKs))

## IDs dropped here would be filtered out in the following steps, so are inconsequential in this analysis. ##

## Recombine full data set with fixed EcologicalSiteIDs
BLM_ind_AIM <- BLM_ind %>% filter(ProjectName!="LMF"|is.na(BLM_ind$ProjectName))
BLM_ind_full <- rbind(BLM_ind_AIM,BLM_ind_LMFEcoSite)

#### Data Filtering - Sandy and shallow sandy ecological sites ####

## Sandy and shallow sandy subset
sandy_ind<-BLM_ind_full %>% 
  filter(EcologicalSiteId=="R042XB012NM"|EcologicalSiteId=="R042XB015NM")

sandy_ind_state<-inner_join(sandy_ind,sandy_state_all,by="PrimaryKey")
sandy_ind_state<-sandy_ind_state %>% relocate(EcologicalSiteId,Phase)

sandyPKs<-sandy_ind_state$PrimaryKey
sandy_aero<-aero %>% 
  filter(PrimaryKey %in% sandyPKs)
sandy_ind_other <- indicators %>% 
  filter(PrimaryKey %in% sandyPKs)

sandy_full<-inner_join(sandy_ind_state,sandy_aero,by="PrimaryKey")
sandy_full$Phase<-as.factor(sandy_full$Phase)
sandy_full<-sandy_full %>% mutate(Phase = recode(Phase,"4.2"="6"))

sandy_full2 <- inner_join(sandy_full,sandy_ind_other,by="PrimaryKey")

sandy_full2$GapCover_100_plus<-sandy_full2$GapCover_101_200.y + sandy_full2$GapCover_200_plus.y


#### Plot Bare Soil Cover ####
## Set custom palette for plots
custom_palette <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#964B00")

## Base plots for bare soil
(plot_bg_Q.legend <- ggplot() +
    geom_point(data=sandy_full,aes(x = BareSoilCover,y = horizontal_flux_total_MD,color=Phase), size = 0.5) +
    scale_color_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland")) +
    geom_smooth(data=sandy_full,aes(x = BareSoilCover,y = horizontal_flux_total_MD),color="darkgray",se=F, size = 0.5) +
    geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .50)), size=0.5) +
    geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .25)), size=0.5, lty=2) +
    geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .75)), size=0.5, lty=2) +
    theme_cowplot(font_size = 6))

(plot_bg_Q.legenda <- ggplot() +
    geom_point(data=sandy_full,aes(x = BareSoilCover,y = horizontal_flux_total_MD,color=Phase), size = 0.5) +
    scale_color_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
    geom_smooth(data=sandy_full,aes(x = BareSoilCover,y = horizontal_flux_total_MD),color="darkgray",se=F, size = 0.5) +
    geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .50)), size=0.5) +
    geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .25)), size=0.5, lty=2) +
    geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .75)), size=0.5, lty=2) +
    scale_y_continuous(name = bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")")) +
    theme_cowplot(font_size = 8))

## Extract legend from base plot
legend <- get_legend(plot_bg_Q.legend +
                       guides(color = guide_legend(nrow = 2)) +
                       theme(legend.position = "bottom"))

#### Plots of bare soil indicator distributions and relationship to horizontal flux model indicator by Ecological State ####
## Full Sandy and Shallow Sandy Ecological Sites 
(plot_bg_Q <- ggplot() +
   geom_point(data=sandy_full,aes(x = BareSoilCover,y = horizontal_flux_total_MD,color=Phase), size = 0.5) +
   scale_color_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
   geom_smooth(data=sandy_full,aes(x = BareSoilCover,y = horizontal_flux_total_MD),color="darkgray",se=F, size = 0.5) +
   geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .50)), size=0.5) +
   geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .25)), size=0.5, lty=2) +
   geom_vline(data=sandy_full,aes(xintercept=quantile(BareSoilCover,probs = .75)), size=0.5, lty=2) +
   scale_y_continuous(name = "Q") +
   theme_cowplot(font_size = 8) +
   theme(axis.text.x = element_blank(),
         axis.title = element_blank()))

## Ecological State 1.2
(plot_bg_Q_1 <- plot_bg_Q + geom_vline(data=sandy_full %>% filter(Phase=="1.2"),aes(xintercept=quantile(BareSoilCover,probs = .50)), color="#E41A1C", size=0.5) +
    geom_vline(data=sandy_full %>% filter(Phase=="1.2"),aes(xintercept=quantile(BareSoilCover,probs = .25)), color="#E41A1C", size=0.5, lty=2) +
    geom_vline(data=sandy_full %>% filter(Phase=="1.2"),aes(xintercept=quantile(BareSoilCover,probs = .75)), color="#E41A1C", size=0.5, lty=2) +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank()))

## Ecological State 3.1
(plot_bg_Q_31 <- plot_bg_Q + geom_vline(data=sandy_full %>% filter(Phase=="3.1"),aes(xintercept=quantile(BareSoilCover,probs = .50)), color="#377EB8", size=0.5) +
    geom_vline(data=sandy_full %>% filter(Phase=="3.1"),aes(xintercept=quantile(BareSoilCover,probs = .25)), color="#377EB8", size=0.5, lty=2) +
    geom_vline(data=sandy_full %>% filter(Phase=="3.1"),aes(xintercept=quantile(BareSoilCover,probs = .75)), color="#377EB8", size=0.5, lty=2) +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank()))

## Ecological State 3.2
(plot_bg_Q_32 <- plot_bg_Q + geom_vline(data=sandy_full %>% filter(Phase=="3.2"),aes(xintercept=quantile(BareSoilCover,probs = .50)), color="#4DAF4A", size=0.5) +
    geom_vline(data=sandy_full %>% filter(Phase=="3.2"),aes(xintercept=quantile(BareSoilCover,probs = .25)), color="#4DAF4A", size=0.5, lty=2) +
    geom_vline(data=sandy_full %>% filter(Phase=="3.2"),aes(xintercept=quantile(BareSoilCover,probs = .75)), color="#4DAF4A", size=0.5, lty=2) +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank()))

## Ecological State 4
(plot_bg_Q_4 <- plot_bg_Q +   geom_vline(data=sandy_full %>% filter(Phase=="4.1"),aes(xintercept=quantile(BareSoilCover,probs = .50)), color="#984EA3", size=0.5) +
    geom_vline(data=sandy_full %>% filter(Phase=="4.1"),aes(xintercept=quantile(BareSoilCover,probs = .25)), color="#984EA3", size=0.5, lty=2) +
    geom_vline(data=sandy_full %>% filter(Phase=="4.1"),aes(xintercept=quantile(BareSoilCover,probs = .75)), color="#984EA3", size=0.5, lty=2) +
    theme(axis.text.x = element_blank(),
          axis.title =element_blank()))

## Ecological State 6
(plot_bg_Q_6 <- plot_bg_Q + geom_vline(data=sandy_full %>% filter(Phase=="6"),aes(xintercept=quantile(BareSoilCover,probs = .50)), color="#FF7F00", size=0.5) +
    geom_vline(data=sandy_full %>% filter(Phase=="6"),aes(xintercept=quantile(BareSoilCover,probs = .25)), color="#FF7F00", size=0.5, lty=2) +
    geom_vline(data=sandy_full %>% filter(Phase=="6"),aes(xintercept=quantile(BareSoilCover,probs = .75)), color="#FF7F00", size=0.5, lty=2) +
    theme(axis.text.x = element_blank(),
          axis.title =element_blank()))

## Ecological State 5
(plot_bg_Q_5 <- plot_bg_Q.legenda + geom_vline(data=sandy_full %>% filter(Phase=="5"),aes(xintercept=quantile(BareSoilCover,probs = .50)), color="#964B00", size=0.5) +
    geom_vline(data=sandy_full %>% filter(Phase=="5"),aes(xintercept=quantile(BareSoilCover,probs = .25)), color="#964B00", size=0.5, lty=2) +
    geom_vline(data=sandy_full %>% filter(Phase=="5"),aes(xintercept=quantile(BareSoilCover,probs = .75)), color="#964B00", size=0.5, lty=2) +
    scale_x_continuous(name = "Bare Soil Cover (%)") +
    theme(axis.title.y = element_blank()))

## Create shared y axis for combined plot 
y.grob <- textGrob(bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")"), gp=gpar(fontsize = 8), rot = 90)


#### Combined plot of bare soil indicator distributions and relationship to horizontal flux model indicator by Ecological State ####
bg_Q_panel<-plot_grid(plot_bg_Q,plot_bg_Q_1,plot_bg_Q_31,plot_bg_Q_32,plot_bg_Q_4,plot_bg_Q_6,plot_bg_Q_5, ncol = 1, rel_heights = c(1,1,1,1,1,1,1.5))

bg_Q_panel2 <- grid.arrange(arrangeGrob(bg_Q_panel, left = y.grob))

legend2 <- plot_grid(NULL, legend, rel_heights = c(0.000001, 1), ncol = 1)

bg_Q_panel3 <-plot_grid(bg_Q_panel2,legend2, ncol = 1, rel_heights = c(7,.5), align = "v", axis = "l", scale = c(1,0.5))

bg_Q_panel3
ggsave("sandy_BG_Q_panel.png", dpi = 1000, height = 4, width = 4, unit = "in")

#### Boxplots of indicators by Ecological State ####

(boxplot_bg.legend <- ggplot() +
   geom_boxplot(data=sandy_full2,aes(x = Phase,y = BareSoilCover.x, fill =Phase, lwd = 0.5), outlier.size = 0.5, lwd = 0.5, fatten = 0.5) +
   scale_fill_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland")) +
   coord_cartesian(ylim = c(0,100)) +
   theme_cowplot(font_size = 6) +
   theme(axis.text.x = element_blank(),
         axis.title.x=element_blank())) 

legend_boxplot <- get_legend(boxplot_bg.legend +
                               guides(color = guide_legend(nrow = 2)) +
                               theme(legend.position = "bottom"))

(boxplot_bg <- ggplot() +
    geom_boxplot(data=sandy_full2,aes(x = Phase,y = BareSoilCover.x, fill =Phase), outlier.size = 0.5, lwd = 0.5, fatten = 0.5) +
    scale_fill_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
    coord_cartesian(ylim = c(0,100)) +
    scale_y_continuous(name = "Bare Soil 
Cover (%)") +
    theme_cowplot(font_size = 6) +
    theme(axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

(boxplot_fc <- ggplot() +
    geom_boxplot(data=sandy_full2,aes(x = Phase,y = TotalFoliarCover.x, fill =Phase), outlier.size = 0.5, lwd = 0.5, fatten = 0.5) +
    scale_fill_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
    coord_cartesian(ylim = c(0,100)) +
    scale_y_continuous(name = "Total Foliar 
Cover (%)") +
    theme_cowplot(font_size = 6) +
    theme(axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

(boxplot_mh <- ggplot() +
    geom_boxplot(data=sandy_full2,aes(x = Phase,y = MeanMaxHeight, fill =Phase), outlier.size = 0.5, lwd = 0.5, fatten = 0.5) +
    scale_fill_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
    scale_y_continuous(name = "Mean Max 
Height (cm)") +
    theme_cowplot(font_size = 6) +
    theme(axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

(boxplot_sg <- ggplot() +
    geom_boxplot(data=sandy_full2,aes(x = Phase,y = ScaledGap_1m, fill =Phase), outlier.size = 0.5, lwd = 0.5, fatten = 0.5) +
    scale_fill_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
    scale_y_continuous(name = "Mean
Scaled Gap") +
    theme_cowplot(font_size = 6) +
    theme(axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

(boxplot_gc <- ggplot() +
    geom_boxplot(data=sandy_full2,aes(x = Phase,y = GapCover_100_plus, fill =Phase), outlier.size = 0.5, lwd = 0.5, fatten = 0.5) +
    scale_fill_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
    scale_y_continuous(name = "Gap Cover 
100 cm+ (%)") +
    theme_cowplot(font_size = 6) +
    theme(axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

(boxplot_q <- ggplot() +
    geom_boxplot(data=sandy_full2,aes(x = Phase,y = horizontal_flux_total_MD, fill =Phase), outlier.size = 0.5, lwd = 0.5, fatten = 0.5) +
    scale_fill_manual(values = custom_palette, name="Ecological State", labels=c("1 Blackgrama Grassland","3.1 Bunchgrass/Mesquite","3.2 Bunchgrass/Mesquite","4 Shrubland","6 Grassy shrubland","5 Exotic perennial grassland"), guide = "none") +
    scale_y_continuous(name = bquote(italic(Q)~"(g" ~m^-1 ~d^-1*")")) +
    theme_cowplot(font_size = 6) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))

combined_boxplot <- plot_grid(boxplot_bg, boxplot_fc, boxplot_mh, boxplot_sg, boxplot_gc, boxplot_q, legend_boxplot, ncol = 1, rel_heights = c(1,1,1,1,1,1.2,0.3), align = "v", axis = "l", scale = c(1,1,1,1,1,1,0.5))  
combined_boxplot

ggsave("boxplot_sandy3.png", dpi = 1000, height = 4, width = 4, unit = "in")
