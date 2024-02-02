# Applying the benchmark concepts including
# linear regression evaluated with minimum curvature radius; piecewise regression; bin variance

# Brandi Wheeler
# 08/25/2023 - updated 02/01/2024

######
# SETUP
######

# libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(segmented)
library(ggpubr)

# Data
#AERO data
AIM_flux<-read.csv("/Users/brandiwheeler/Documents/Masters/WindErosionBenchmarks-main/data/indicators/aero.csv")

#Core methods data
indicators <- read.csv("/Users/brandiwheeler/Documents/Masters/WindErosionBenchmarks-main/data/indicators/indicators.csv")

#Assigning Q (horizontal flux) to core methods data
#Q md
indicators$hflux_md<-AIM_flux$horizontal_flux_total_MD[match(indicators$PrimaryKey,AIM_flux$PrimaryKey)]

#Q upi
indicators$hflux_upi<-AIM_flux$horizontal_flux_total_UPI[match(indicators$PrimaryKey,AIM_flux$PrimaryKey)]

#Q lpi
indicators$hflux_lpi<-AIM_flux$horizontal_flux_total_LPI[match(indicators$PrimaryKey,AIM_flux$PrimaryKey)]


#log transforming Q - log = natural log

# first need to set values below 0.1 to 0 to ensure values aren't too inflated when we add 1 to
# make sure we aren't taking log(0)
indicators$hflux_upi_log <- ifelse(indicators$hflux_upi > 0.1, indicators$hflux_upi, 0)
indicators$hflux_lpi_log <- ifelse(indicators$hflux_lpi > 0.1, indicators$hflux_lpi, 0)
indicators$hflux_md_log <- ifelse(indicators$hflux_md > 0.1, indicators$hflux_md, 0)

# now can transform flux values plus one
indicators$loguq<-log(indicators$hflux_upi_log+1)
indicators$loglq<-log(indicators$hflux_lpi_log+1)
indicators$logh<-log(indicators$hflux_md_log+1)

indicators<- indicators[!is.na(indicators$logh),]


###
# 1. Minimum radius of curvature
###

# We expect wind erosion to have an  
# exponential relationship with ecosystem characteristics (e.g., Burri et al., 2010) such as 
# bare ground cover/total foliar cover and canopy gap cover - 
# we can determine the exponential model equation for each indicator and then use minimum radius of curvature, MCR,
# to identify the benchmark

#### GapCover_100 +

##### Look at the fit with exponential curve

# xexponential = semi-log or linear-log meaning Q is log and BG is not
indicators$GapCover_100_plus <- indicators$GapCover_101_200 + indicators$GapCover_200_plus
GapCover_100_plus_expon <- lm(indicators$logh ~ indicators$GapCover_100_plus)

plot(indicators$logh ~ indicators$GapCover_100_plus)

crPlot(GapCover_100_plus_expon, variable = "indicators$GapCover_100_plus")

# blue dashed is expected and pink is based on observations

#scale-location and partial residuals to check for constant 
# variance of the errors

plot(GapCover_100_plus_expon, which = 3)

# with summary can check Rsq and get the equation of the line
summary(GapCover_100_plus_expon)

# Rsq goodness of fit - if p-value: < 0.5 we can conclude the model could have come from 
# an exponential fit

# log(y) = (0.037614*(x)) - 0.065557)

###### Now calculating the first and second derivative to calculate the MCR

#the derivative is ( 0.037614 * exp(0.037614*x))
# the second derivative is (0.001414813 * exp(0.037614*x))

# calculating the MCR based on the linear regression for
# each whole number in our datan set
Xs <- c(1:96)
Xs <- sort(Xs)
dat <- data.frame(x = Xs, d1 = numeric(length(Xs)), d2_np = numeric(length(Xs)),
                  d2_prime = numeric(length(Xs)), R = numeric(length(Xs)))

for(i in 1:length(Xs)) {
  dat$d1[i] <- (0.037614 * exp(0.037614*dat$x[i]))
  dat$d2[i] <- (0.001414813 * exp(0.037614*dat$x[i]))
  dat$R[i] <- ((1+dat$d1[i]^2)^(3/2))/dat$d2[i]
  dat$R_prime[i] <- sqrt(dat$R[i]^2)
  
}

Rmin <- min(dat$R, na.rm = T)
Rmin_x <- subset(dat, R == Rmin)
Rmin_x$x

# the MCR is 78

#### for graphing later on in the code

rlab <- expression(paste("",R^2," = 0.4422 "))
eq <- expression(paste("Linear Regression\nLn (y) = 0.037614x - 0.065557"))

# solving Y for every X to get our curve overlaying the scatterplots
Xs <- unique(indicators$GapCover_100_plus)
line <- as.data.frame(Xs)
min_line <- line %>% mutate(Q = exp((0.037614*Xs)- 0.065557))


# solving for Y (Q) at 78 gap cover

exp((0.037614*78)- 0.065557)


############
# 2. Segmented Regression
############
# can determine where there is a significant change in the exponential relationship
# if use a segmented/piecewise regression consisting of two linear regressions

# package https://cran.r-project.org/web/packages/segmented/segmented.pdf

# code from https://www.statology.org/piecewise-regression-in-r/

#### Gap 100+

fit <- lm(hflux_md ~ GapCover_100_plus, data=indicators)


#fit piecewise regression model to original model, estimating a breakpoint at x=10
segmented.fit <- segmented(fit, seg.Z = ~GapCover_100_plus, psi=10)

summary(segmented.fit)

GapCover_100_plus_BP <-  73.042

# If x ≤ GapCover_100_200_BP:  y = -0.91646  +  0.28678*(x)

# If x > GapCover_100_200_BP:  y = -0.91646 +  0.28678*(GapCover_100_plus_BP) + ( 0.28678+3.87682 )*(x-GapCover_100_plus_BP)

# solving Y for every X to get our curve overlaying the scatterplots

Xs <- unique(indicators$GapCover_100_plus)
line <- as.data.frame(Xs)
seg_line <- line %>% mutate(Q = ifelse(Xs <= 73.042, -0.91646  +  0.28678*(Xs),
                                       -0.91646 +  0.28678*(GapCover_100_plus_BP) + ( 0.28678+3.87682 )*(Xs-GapCover_100_plus_BP)))
# solving for Q 
-0.91646  +  0.28678*(73.042)

#20.03052


############
# 3. Linear and segmented regression approaches single graph
############

substitute(paste(italic("Q")))

gap_regressions <- ggplot() + geom_point(data = indicators, aes(x = GapCover_100_plus, y = hflux_md), size=1)+
  geom_line(data = min_line, aes(x = Xs, y = Q), color = "indianred4", linewidth = 0.6)+
  labs(x = "Gap Cover 100+ cm (%)", y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_segment(aes(x=78, y=17.60768, xend=78, yend=218), linetype="dashed",col = "indianred4")+
  geom_label(aes(x=78, y=219,  label = "Linear"),
             label.size = NA, color = "indianred4")+
  geom_line(data = seg_line, aes(x = Xs, y = Q), color = "royalblue3", size = 0.7)+
  geom_segment(aes(x=73.042, y=20.03052, xend=73.042, yend=155), linetype="dashed",col = "royalblue3")+
  geom_label(aes(x=73.042, y=154,  label = "Segmented"),
             label.size = NA, color = "royalblue3")+
  ylim(0,400)+xlim(0,100.1)

gap_regressions

#### BareSoilCover

####
# 1. Linear regression - MCR
###

##### Look at the fit with exponential curve

# exponential = semi-log or linear-log meaning Q is log and BG is not
BareSoilCover_expon <- lm(indicators$logh ~ indicators$BareSoilCover)

plot(indicators$logh ~ indicators$BareSoilCover)

crPlot(BareSoilCover_expon, variable = "indicators$BareSoilCover")

# blue dashed is expected and pink is based on observations

#scale-location and partial residuals to check for constant 
# variance of the errors 

plot(BareSoilCover_expon, which = 3)

# summary to review Rsq and get equation of the line
summary(BareSoilCover_expon)

# getting the equation of the line

summary(BareSoilCover_expon)

# log(y) = (0.049735*(x)) + 0.100596 

###### Now calculating the first and second derivative to calculate the MCR

#the derivative is (0.049735 * exp(0.049735*x))
# the second derivative is (0.00247357 * exp(0.049735*x))

# MCR from regression using the whole number Xs
Xs <- c(1:92)
Xs <- sort(Xs)
dat <- data.frame(x = Xs, d1 = numeric(length(Xs)), d2_np = numeric(length(Xs)),
                  d2_prime = numeric(length(Xs)), R = numeric(length(Xs)))

for(i in 1:length(Xs)) {
  dat$d1[i] <- (0.049735 * exp(0.049735*dat$x[i]))
  dat$d2[i] <- (0.00247357 * exp(0.049735*dat$x[i]))
  dat$R[i] <- ((1+dat$d1[i]^2)^(3/2))/dat$d2[i]
  dat$R_prime[i] <- sqrt(dat$R[i]^2)
  
}

Rmin <- min(dat$R, na.rm = T)
Rmin_x <- subset(dat, R == Rmin)
Rmin_x$x

# 53 is the X MCR

#### for graphing later on in the code

rlab <- expression(paste("",R^2," = 0.5614  "))
eq <- expression(paste("Linear Regression\nLn (y) = 0.049735x + 0.100596"))

# solving Y for every X to get our curve overlaying the scatterplots
Xs <- unique(indicators$BareSoilCover)
line <- as.data.frame(Xs)
min_line <- line %>% mutate(Q = exp((0.049735*Xs)+0.100596 ))


# solving for Y (Q) at 53

exp((0.049735*53)+0.100596 )


##############################
# 2. Segmented regression
#############################
#### Bare soil cover

fit <- lm(hflux_md ~ BareSoilCover, data=indicators)


#fit piecewise regression model to original model, estimating a breakpoint at x=50
segmented.fit <- segmented(fit, seg.Z = ~BareSoilCover, psi=50)

summary(segmented.fit)

# 50.993

BG_BP <-  50.993

# If x ≤ BG_BP:  y = -2.0868   +  0.4567 *(x)

# If x > BG_BP:  y = -2.0868  +  0.4567 *(BG_BP) + (0.4567 +2.1796)*(x-BG_BP)

# solving Y for every X to get our curve overlaying the scatterplots

Xs <- unique(indicators$BareSoilCover)
line <- as.data.frame(Xs)
seg_line <- line %>% mutate(Q = ifelse(Xs <= BG_BP, -2.0868   +  0.4567*(Xs),
                                       -2.0868  +  0.4567 *(BG_BP) + (0.4567 +2.1796)*(Xs-BG_BP)))
# solving for the AERO bench 
-2.0868   +  0.4567*(50.993)
#21.2017

#plot original data
plot(indicators$BareSoilCover, indicators$hflux_md, pch=16, col='steelblue', 
     ylab="Horizontal Sediment Flux", xlab="Bare Soil Cover (%)")

#add segmented regression model
plot(segmented.fit, add=T)



############################################################
# 3. Graphing linear and segmented regression approaches
############################################################

BG_regressions <- ggplot() + geom_point(data = indicators, aes(x = BareSoilCover, y = hflux_md), size=1)+
  geom_line(data = min_line, aes(x = Xs, y = Q), color = "indianred4", linewidth = 0.6)+
  labs(x = "Bare Soil (%)", y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_line(data = seg_line, aes(x = Xs, y = Q), color = "royalblue3", size = 0.7)+
  geom_segment(aes(x=53, y=15.43366, xend=53, yend=215), linetype="dashed",col = "indianred4")+
  geom_label(aes(x=53, y=216,  label = "Linear"),
             label.size = NA, color = "indianred4")+
  geom_segment(aes(x=50.993, y=21.2017, xend=50.993, yend=150), linetype="dashed",col = "royalblue3")+
  geom_label(aes(x=50.993, y=149,  label = "Segmented"),
             label.size = NA, color = "royalblue3")+
  ylim(0,400)+xlim(0,100.1)

BG_regressions


#### TotalFoliarCover

#################################
# 1. Linear regression and MCR
#################################

##### Look at the fit with exponential curve

# exponential decay
indicators$TFC <- indicators$TotalFoliarCover
TotalFoliarCover_expon <- lm(indicators$logh ~ indicators$TFC)

plot(indicators$logh ~ indicators$TFC)

crPlot(TotalFoliarCover_expon, variable = "indicators$TFC")

# blue dashed is expected and pink is based on observations


#scale-location and partial residuals to check for constant 
# variance of the errors 

plot(TotalFoliarCover_expon, which = 3)

# summary to review Rsq and get the equation of the line
summary(TotalFoliarCover_expon)

# log(y) = (-0.058398 *(x)) + 3.645837 )

#exp(-0.058398 *(x)) + 3.645837 )
# MCR from regression and the nearest whole number X
Xs <- c(1:95)
Xs <- sort(Xs)
dat <- data.frame(x = Xs, d1 = numeric(length(Xs)), d2_np = numeric(length(Xs)),
                  d2_prime = numeric(length(Xs)), R = numeric(length(Xs)))

for(i in 1:length(Xs)) {
  dat$d1[i] <- (0.058398 * exp(0.058398*dat$x[i]))
  dat$d2_np[i] <- (0.003410326 * exp(0.058398*dat$x[i]))
  dat$d2_prime[i] <- sqrt(dat$d2_np[i]^2)
  dat$R[i] <- ((1+dat$d1[i]^2)^(3/2))/dat$d2_prime[i]
  
  
}

Rmin <- min(dat$R, na.rm = T)
Rmin_x <- subset(dat, R == Rmin)
Rmin_x$x

# 43 is MCR x

#### for graphing later on in the code

rlab <- expression(paste("",R^2," = 0.5113 "))
eq <- expression(paste("Linear Regression\nLn (y) = -0.058398x + 3.645837 "))

# solving Y for every X to get our curve overlaying the scatterplots
Xs <- c(0:100)
line <- as.data.frame(Xs)
min_line <- line %>% mutate(Q = exp((-0.058398*Xs)+3.645837 ))



# solving for Y (Q) at 43

exp((-0.058398*43)+3.645837 )

# 3.110312

##############################
#2. Segmented regression
#############################
#### TFC

fit <- lm(hflux_md ~ TFC, data=indicators)


#fit piecewise regression model to original model, estimating a breakpoint at x=450
segmented.fit <- segmented(fit, seg.Z = ~TFC, psi=45)

summary(segmented.fit)

# 19.274


TFC_BP <-  19.274

# If x ≤ BG_BP:  y = 156.7657 -7.4019 *(x)

# If x > BG_BP:  y = 156.7657 -7.4019 *(BG_BP) + (-7.4019 +7.0636 )*(x-BG_BP)

# solving Y for every X to get our curve overlaying the scatterplots

Xs <- unique(indicators$TotalFoliarCover)
line <- as.data.frame(Xs)
seg_line <- line %>% mutate(Q = ifelse(Xs <= TFC_BP, 156.7657 -7.4019*(Xs),
                                       156.7657 -7.4019 *(TFC_BP) + (-7.4019 +7.0636 )*(Xs-TFC_BP)))
# solving for the AERO bench 
156.7657-7.4019*( 19.274)
#14.10148



###########################################################
#3. Graphing the linear and segmented regression approaches
###########################################################

# in order to graph the x axis ranging from 0 to 100, the TFC value is actually 100-

TFC_regressions <- ggplot() + geom_point(data = indicators, aes(x = TFC, y = hflux_md), size=1)+
  geom_line(data = min_line, aes(x = Xs, y = Q), color = "indianred4", size = 0.7)+
  labs(x = "Total Foliar Cover (%)", y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_segment(aes(x=(19.274), y=14.10091, xend=(19.274), yend=195), linetype="dashed",col = "royalblue3")+
  geom_label(aes(x=(19.274), y=194,  label = "Segmented"),
             label.size = NA, color = "royalblue3")+
  geom_segment(aes(x=43, y=3.110312, xend=43, yend=195), linetype="dashed",col = "indianred4")+
  geom_label(aes(x=43, y=194,  label = "Linear"),
             label.size = NA, color = "indianred4")+
  geom_line(data = seg_line, aes(x = Xs, y = Q), color = "royalblue3", size = 0.7)+
  ylim(0,400)+xlim(0,100.1)

TFC_regressions



#### Mean ScaledGap with removal of outlier

#################################
# 1. Linear regression and MCR
#################################

##### Look at the fit with exponential curve

# exponential = semi-log or linear-log meaning Q is log and Hgt is not
indicators2 <- indicators %>% filter(ScaledGap < 100)
ScaledGap_expon <- lm(indicators2$logh ~ indicators2$ScaledGap)

plot(indicators2$logh ~ indicators2$ScaledGap)

crPlot(ScaledGap_expon, variable = "indicators2$ScaledGap")

# blue dashed is expected and pink is based on observations

#scale-location and partial residuals to check for constant 
# variance of the errors 

plot(ScaledGap_expon, which = 3)

# summary to review Rsq and get the equation of the line
summary(ScaledGap_expon)

# log(y) = (0.193808*(x)) +0.662335

###### Now calculating the first and second derivative to calculate the MCR

#the derivative is (0.193808 * exp(0.193808*x))
# the second derivative is (0.03756154 * exp(0.193808*x))

# MCR line for the nearest whole number X
Xs <- c(1:69)
Xs <- sort(Xs)
dat <- data.frame(x = Xs, d1 = numeric(length(Xs)), d2_np = numeric(length(Xs)),
                  d2_prime = numeric(length(Xs)), R = numeric(length(Xs)))

for(i in 1:length(Xs)) {
  dat$d1[i] <- (0.193808 * exp(0.193808*dat$x[i]))
  dat$d2[i] <- (0.03756154 * exp(0.193808*dat$x[i]))
  dat$R[i] <- ((1+dat$d1[i]^2)^(3/2))/dat$d2[i]
  dat$R_prime[i] <- sqrt(dat$R[i]^2)
  
}

Rmin <- min(dat$R, na.rm = T)
Rmin_x <- subset(dat, R == Rmin)
Rmin_x$x

# MCR, x= 7
#### for graphing later on in the code

eq <- expression(paste("Linear Regression\nLn (y) = 0.193808x + 0.662335 "))

# solving Y for every X to get our curve overlaying the scatterplots
Xs <- c(0:69)
line <- as.data.frame(Xs)
min_line <- line %>% mutate(Q = exp((0.193808*Xs)+0.662335 ))



# solving for Y (Q) at 7

exp((0.193808*7)+0.662335 )
# 7.530723

###############
#2. Segmented regression
##############
#### Mean Scaled Gap

fit <- lm(hflux_md ~ ScaledGap, data=indicators2)


#fit piecewise regression model to original model, estimating a breakpoint at x=450
segmented.fit <- segmented(fit, seg.Z = ~ScaledGap, psi=1)

summary(segmented.fit)

# 2.658

#plot original data
plot(indicators$ScaledGap, indicators$hflux_md, pch=16, col='steelblue', 
     ylab="Horizontal Sediment Flux", xlab="Mean Scaled Gap")

#add segmented regression model
plot(segmented.fit, add=T)



SG_BP <-  2.658

# If x ≤ BG_BP:  y = -0.9623   +  0.8915 *(x)

# If x > BG_BP:  y = -0.9623  +  0.8915 *(BG_BP) + (0.8915 +2.1796)*(x-BG_BP)

# solving Y for every X to get our curve overlaying the scatterplots

Xs <- unique(indicators$ScaledGap)
line <- as.data.frame(Xs)
seg_line <- line %>% mutate(Q = ifelse(Xs <= SG_BP, -0.9623   +  0.8915*(Xs),
                                       -0.9623  +  0.8915 *(SG_BP) + (0.8915 +5.4419)*(Xs-SG_BP)))
# solving for the AERO bench 
-0.9623   +  0.8915*(2.658)
#1.407307


###############
#3. Graphing the linear and segmented regression approaches
##############

scaled_regressions<- ggplot() + geom_point(data = indicators, aes(x = ScaledGap, y = hflux_md), size=1)+
  geom_line(data = min_line, aes(x = Xs, y = Q), color = "indianred4", size = 0.7)+
  labs(x = "Mean Scaled Gap", y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_segment(aes(x=7, y=7.530723, xend=7, yend=205), linetype="dashed",col = "indianred4")+
  geom_label(aes(x=7, y=204,  label = "Linear"),
             label.size = NA, color = "indianred4")+
  
  geom_segment(aes(x=2.658, y=1.407307, xend=2.658, yend=142), linetype="dashed",col = "royalblue3")+
  geom_label(aes(x=4, y=142,  label = "Segmented"),
             label.size = NA, color = "royalblue3")+
  geom_line(data = seg_line, aes(x = Xs, y = Q), color = "royalblue3", size = 0.7)+
  ylim(0,400)+xlim(0,40.1)

scaled_regressions


################################################################################################################

# Variance between bins of points

# ~10% bin spacing

#### Gap Cover 100+

# evaluate bins needed and set
summary(indicators$GapCover_100_plus)


indicators$GapCover_100_plus_bins<- ifelse(indicators$GapCover_100_plus <= 10, "1",
                                           ifelse(indicators$GapCover_100_plus > 10 & indicators$GapCover_100_plus <= 20, "2",
                                                  ifelse(indicators$GapCover_100_plus >20 & indicators$GapCover_100_plus <= 30,"3",
                                                         ifelse(indicators$GapCover_100_plus >30 & indicators$GapCover_100_plus <= 40,"4",
                                                                ifelse(indicators$GapCover_100_plus >40 & indicators$GapCover_100_plus <= 50,"5",
                                                                       ifelse(indicators$GapCover_100_plus >50 & indicators$GapCover_100_plus <= 60,"6",
                                                                              ifelse(indicators$GapCover_100_plus >60 & indicators$GapCover_100_plus <= 70,"7",
                                                                                     ifelse(indicators$GapCover_100_plus >70 & indicators$GapCover_100_plus <= 80,"8",
                                                                                            ifelse(indicators$GapCover_100_plus >80 & indicators$GapCover_100_plus <= 90,"9","10")))))))))

# check count per bin to ensure bins do not need to be adjusted
agg_tbl <- indicators %>% group_by(GapCover_100_plus_bins) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

agg_tbl


# calculate the variance of bins

indicators_var <- indicators %>% group_by (GapCover_100_plus_bins) %>% mutate(variance = var(hflux_md))

indicators_var <- indicators_var[!duplicated(indicators_var$GapCover_100_plus_bins),]

# graphing

# line
indicators_var$GapCover_100_plus_bins <- as.numeric(indicators_var$GapCover_100_plus_bins)
my_labels <- c(5,15,25,35,45,55,65,75,85,95)
Gap_variance <- ggplot(indicators_var, aes(x = GapCover_100_plus_bins, y = variance)) + 
  geom_point()+
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(x = "Gap Cover 100 cm + (%)", y = expression(paste("Variance of", italic("Q"), " ( ", g^2, " ", m^-2, " ", d^-2,")")))+
  scale_x_continuous(breaks=seq(1,10,1),labels=my_labels)+
  geom_segment(aes(x=5.5, y=Inf, xend=5.5, yend=-Inf), linetype="dashed",col = "black")

#geom segment is placed between the 5th and 6th bin to represent the highest value 
# within the 5th bin (50). This has visually been identified as the benchmark after which there appears to be a marked increase
# in variance of Q
Gap_variance

# boxplot

indicators$GapCover_100_plus_bins <- factor(indicators$GapCover_100_plus_bins , levels=c(1,2,3,4,5,6,7,8,9,10))
gap_variance_box <- ggplot(indicators, aes(x = factor(GapCover_100_plus_bins), y = hflux_md)) + 
  geom_rect(aes(xmin = 5.5, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "gray95", alpha = 0.9)+
  geom_boxplot()+
  stat_summary(mapping = aes(group = GapCover_100_plus_bins),
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "errorbar",
               color = "indianred4",
               width = .3
  )+
  labs(x = "Gap Cover 100 cm + (%)", y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_x_discrete(labels=my_labels)+
  geom_segment(aes(x=5.5, y=Inf, xend=5.5, yend=-Inf), linetype="dashed",col = "black")

gap_variance_box



#### BG

# evaluating the number of bins we need and setting
summary(indicators$BareSoilCover)


# for our example there was only 1 > 90 so included the 91% cover point in the final bin
indicators$BareSoilCover_bins<- ifelse(indicators$BareSoilCover <= 10, "1",
                                       ifelse(indicators$BareSoilCover > 10 & indicators$BareSoilCover <= 20, "2",
                                              ifelse(indicators$BareSoilCover >20 & indicators$BareSoilCover <= 30,"3",
                                                     ifelse(indicators$BareSoilCover >30 & indicators$BareSoilCover <= 40,"4",
                                                            ifelse(indicators$BareSoilCover >40 & indicators$BareSoilCover <= 50,"5",
                                                                   ifelse(indicators$BareSoilCover >50 & indicators$BareSoilCover <= 60,"6",
                                                                          ifelse(indicators$BareSoilCover >60 & indicators$BareSoilCover <= 70,"7",
                                                                                 ifelse(indicators$BareSoilCover >70 & indicators$BareSoilCover <= 80,"8","9"))))))))

# check count per bin
agg_tbl <- indicators %>% group_by(BareSoilCover_bins) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

agg_tbl


# calculate the variance of bins

indicators_var <- indicators %>% group_by (BareSoilCover_bins) %>% mutate(variance = var(hflux_md))

indicators_var <- indicators_var[!duplicated(indicators_var$BareSoilCover_bins),]

# graphing

# line
indicators_var$BareSoilCover_bins <- as.numeric(indicators_var$BareSoilCover_bins)
my_labels <- c(5,15,25,35,45,55,65,75,85)
BG_variance <- ggplot(indicators_var, aes(x = BareSoilCover_bins, y = variance)) + 
  geom_point()+
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(x = "Bare Soil (%)", y = expression(paste("Variance of", italic("Q"), " ( ", g^2, " ", m^-2, " ", d^-2,")")))+
  scale_x_continuous(breaks=seq(1,9,1),labels=my_labels)+
  geom_segment(aes(x=4.5, y=Inf, xend=4.5, yend=-Inf), linetype="dashed",col = "black")

#geom segment is placed between the 4th and 5th bin to represent the highest value 
# within the 4th bin (40). This has visually been identified as the benchmark after which there appears to be a marked increase
# in variance of Q

BG_variance

# boxplot

indicators$BareSoilCover_bins <- factor(indicators$BareSoilCover_bins , levels=c(1,2,3,4,5,6,7,8,9))
BG_variance_box<-ggplot(indicators, aes(x = factor(BareSoilCover_bins), y = hflux_md)) + 
  geom_rect(aes(xmin = 4.5, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "gray95", alpha = 0.9)+
  geom_boxplot()+
  stat_summary(mapping = aes(group = BareSoilCover_bins),
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "errorbar",
               color = "indianred4",
               width = .3
  )+
  labs(x = "Bare Soil (%)", y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_x_discrete(labels=my_labels)+
  geom_segment(aes(x=4.5, y=Inf, xend=4.5, yend=-Inf), linetype="dashed",col = "black")

BG_variance_box


#### TFC

#evaluating the number of bins needed and setting
summary(indicators$TotalFoliarCover)

# for our example there were only 3 in the 90+ bin so included them in the final bin
indicators$TotalFoliarCover_bins<- ifelse(indicators$TotalFoliarCover <= 10, "1",
                                          ifelse(indicators$TotalFoliarCover > 10 & indicators$TotalFoliarCover <= 20, "2",
                                                 ifelse(indicators$TotalFoliarCover >20 & indicators$TotalFoliarCover <= 30,"3",
                                                        ifelse(indicators$TotalFoliarCover >30 & indicators$TotalFoliarCover <= 40,"4",
                                                               ifelse(indicators$TotalFoliarCover >40 & indicators$TotalFoliarCover <= 50,"5",
                                                                      ifelse(indicators$TotalFoliarCover >50 & indicators$TotalFoliarCover <= 60,"6",
                                                                             ifelse(indicators$TotalFoliarCover >60 & indicators$TotalFoliarCover <= 70,"7",
                                                                                    ifelse(indicators$TotalFoliarCover >70 & indicators$TotalFoliarCover <= 80,"8","9"))))))))

# check count per bin
agg_tbl <- indicators %>% group_by(TotalFoliarCover_bins) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

agg_tbl


# calculate the variance of bins

indicators_var <- indicators %>% group_by (TotalFoliarCover_bins) %>% mutate(variance = var(hflux_md))

indicators_var <- indicators_var[!duplicated(indicators_var$TotalFoliarCover_bins),]

# graphing

# line
indicators_var$TotalFoliarCover_bins <- as.numeric(indicators_var$TotalFoliarCover_bins)
my_labels <- c(5,15,25,35,45,55,65,75,85)
TFC_variance <- ggplot(indicators_var, aes(x = TotalFoliarCover_bins, y = variance)) + 
  geom_point()+ 
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(x = "Total Foliar Cover (%)", y = expression(paste("Variance of", italic("Q"), " ( ", g^2, " ", m^-2, " ", d^-2,")")))+
  scale_x_continuous(breaks=seq(1,9,1),labels=my_labels)+
  geom_segment(aes(x=3.5, y=Inf, xend=3.5, yend=-Inf), linetype="dashed",col = "black")

#geom segment is placed between the 3rd and 4th bin to represent the highest value 
# within the 3rd bin (30). This has visually been identified as the benchmark after which there appears to be a marked increase
# in variance of Q

TFC_variance

# boxplot

indicators$TotalFoliarCover_bins <- factor(indicators$TotalFoliarCover_bins , levels=c(1,2,3,4,5,6,7,8,9))
TFC_variance_box <- ggplot(indicators, aes(x = factor(TotalFoliarCover_bins), y = hflux_md)) + 
  geom_rect(aes(xmin = -Inf, xmax = 3.5, ymin = -Inf, ymax = Inf), fill = "gray95", alpha = 0.9)+
  geom_boxplot()+
  stat_summary(mapping = aes(group = TotalFoliarCover_bins),
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "errorbar",
               color = "indianred4",
               width = .3
  )+
  labs(x = "Total Foliar Cover (%)", y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_x_discrete(labels=my_labels)+
  geom_segment(aes(x=3.5, y=Inf, xend=3.5, yend=-Inf), linetype="dashed",col = "black")


TFC_variance_box


#### ScaledGap


# logged Mean Scaled Gap
# removing the outlier point
indicators2 <- indicators %>% filter(ScaledGap < 140)

indicators2$logged_ScaledGap <- 1+indicators2$ScaledGap
indicators2$logged_ScaledGap <- log10(indicators2$logged_ScaledGap)

#evaluating the number of bins needed and setting
summary(indicators2$logged_ScaledGap)


indicators2$logged_ScaledGap_bins<- ifelse(indicators2$logged_ScaledGap <= 0.22, "1",
                                           ifelse(indicators2$logged_ScaledGap > 0.22 & indicators2$logged_ScaledGap <= 0.44, "2",
                                                  ifelse(indicators2$logged_ScaledGap >0.44 & indicators2$logged_ScaledGap <= 0.66,"3",
                                                         ifelse(indicators2$logged_ScaledGap >0.66 & indicators2$logged_ScaledGap <= 0.88,"4",
                                                                ifelse(indicators2$logged_ScaledGap >0.88 & indicators2$logged_ScaledGap <= 1.1,"5",
                                                                       ifelse(indicators2$logged_ScaledGap >1.1 & indicators2$logged_ScaledGap <= 1.33,"6","7"))))))

# check count per bin
agg_tbl <- indicators2 %>% group_by(logged_ScaledGap_bins) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

agg_tbl


# calculate the variance of bins

indicators2_var <- indicators2 %>% group_by (logged_ScaledGap_bins) %>% mutate(variance = var(hflux_md))

indicators2_var <- indicators2_var[!duplicated(indicators2_var$logged_ScaledGap_bins),]

# graphing

# line
indicators2_var$logged_ScaledGap_bins <- as.numeric(indicators2_var$logged_ScaledGap_bins)
my_labels <- c(0.11,0.33,0.55,0.77,0.99,1.21,1.7)
logged_scaledgap_variance <- ggplot(indicators2_var, aes(x = logged_ScaledGap_bins, y = variance)) + 
  geom_point()+ 
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  labs(x = expression(paste("Log" [10], " Mean Scaled Gap")), y = expression(paste("Variance of", italic("Q"), " ( ", g^2, " ", m^-2, " ", d^-2,")")))+
  scale_x_continuous(breaks=seq(1,7,1),labels=my_labels)+
  geom_segment(aes(x=5.5, y=Inf, xend=5.5, yend=-Inf), linetype="dashed",col = "black")
#geom segment is placed between the 5th and 6th bin to represent the highest value 
# within the 5th bin (1.1). This has visually been identified as the benchmark after which there appears to be a marked increase
# in variance of Q

logged_scaledgap_variance


# boxplot

indicators2$logged_ScaledGap_bins <- factor(indicators2$logged_ScaledGap_bins , levels=c(1,2,3,4,5,6,7))
log_scaledgap_variance_box <- ggplot(indicators2, aes(x = factor(logged_ScaledGap_bins), y = hflux_md)) + 
  geom_rect(aes(xmin = 5.5, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "gray95", alpha = 0.9)+
  geom_boxplot()+
  stat_summary(mapping = aes(group = logged_ScaledGap_bins),
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "errorbar",
               color = "indianred4",
               width = .3
  )+
  labs(x = expression(paste("Log" [10], " Mean Scaled Gap")), y = expression(paste(italic("Q"), " (g ", m^-1, " ", d^-1,")"))) +
  theme(text=element_text(size=12),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  scale_x_discrete(labels=my_labels)+
  geom_segment(aes(x=5.5, y=Inf, xend=5.5, yend=-Inf), linetype="dashed",col = "black")
  


log_scaledgap_variance_box

# writing pdfs

# pdf 1 - regressions

ggarrange(BG_regressions, TFC_regressions, gap_regressions, scaled_regressions, 
          labels = c("", "", "",""),
          ncol = 2, nrow = 2)

ggsave("benchmark_regressions.pdf", width = 21.59,  height = 15.24,  units = "cm")


# pdf 2 - variance

ggarrange(BG_variance, TFC_variance, Gap_variance, logged_scaledgap_variance, 
          labels = c("", "", "",""),
          ncol = 2, nrow = 2)

ggsave("benchmark_variance.pdf", width = 21.59,  height = 15.24,  units = "cm")


# pdf 3 - boxplots with mean +- SD

ggarrange(BG_variance_box, TFC_variance_box, gap_variance_box, log_scaledgap_variance_box, 
          labels = c("", "", "",""),
          ncol = 2, nrow = 2)

ggsave("benchmark_boxplot.pdf", width = 21.59,  height = 15.24,  units = "cm")




