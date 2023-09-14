#### CONFIG ####################################################################
# To do plotting
library(ggplot2)
# To grab point location information from the Landscape Data Commons
# devtools::install_github("landscape-data-commons/trex", build_vignettes = TRUE)
library(trex)
# To assemble the final figure
library(patchwork)

# This is the base folder which contains the folders data and figures
filepath <- "C:/Users/Nelson/Documents/Projects/benchmark_paper_webb"

# Filenames for things in the data folder that we'll need
# The AERO data filename
aero_filename <- "aero.csv"
# The table of indicators filename
indicator_filename <- "indicators_20221022.csv"
# The polygons for MLRAs filename
mlra_filename <- "MLRA_52"
# The ecological site group data filename
esg_filename <- "sandy_esg_data.csv"


#### READING AND MUNGING #######################################################
aero_filepath <- paste0(filepath,
                        "/data/",
                        aero_filename)
aero_data <- read.csv(file = aero_filepath,
                      stringsAsFactors = FALSE)
indicator_filepath <- paste0(filepath,
                             "/data/",
                             indicator_filename)
indicator_data <- read.csv(indicator_filepath,
                           stringsAsFactors = FALSE)

data <- dplyr::left_join(x = indicator_data,
                         y = aero_data,
                         by = "PrimaryKey")

# These are just data for the Sandy & Shallow Sandy ESG
esg_filepath <- paste0(filepath,
                       "/data/",
                       esg_filename)
esg_table <- read.csv(file = esg_filepath,
                      stringsAsFactors = FALSE)

# Make an ESG lookup table with all assumed to be "Unknown" then assign to the
# Sandy & Shallow Sandy ESG for all PrimaryKeys occuring in the data from that ESG
esg_lut <- data.frame(PrimaryKey = data$PrimaryKey)
esg_lut$esg <- "Unknown"
esg_lut$esg[esg_lut$PrimaryKey %in% esg_table$PrimaryKey] <- "Sandy and Shallow Sandy Ecological Site Group"

mlra_filepath <- paste0(filepath,
                        "/data/",
                        mlra_filename)
mlra_sf <- sf::st_read(dsn = paste0(filepath, "/data"),
                       layer = mlra_filename)


# Primary keys can be used to get the info from the LDC
primary_keys <- unique(data$PrimaryKey)

# Grab the info about location and ecosite
ldc_headers <- trex::fetch_ldc(keys = primary_keys,
                               key_type = "PrimaryKey",
                               data_type = "header")

# Convert the headers into a spatial object so we can compare to mlra_sf
ldc_headers_sf <- sf::st_as_sf(x = ldc_headers,
                               coords = c("Longitude_NAD83",
                                          "Latitude_NAD83"),
                               crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")

# Make a lookup table of MLRA attributes
mlra_lut <- sf::st_intersection(x = ldc_headers_sf[, "PrimaryKey"],
                                y = sf::st_transform(mlra_sf,
                                                     crs = sf::st_crs(ldc_headers_sf)))


# Finally, let's join the lookup tables to the data
attribute_lut <- dplyr::left_join(x = esg_lut,
                                  y = mlra_lut)

data <- dplyr::left_join(x = data,
                         y = attribute_lut)

# STUPID LITTLE CORRECTION
data$MeanGap_1m[is.na(data$MeanGap_1m)] <- 0
data$ScaledGap_1m[is.na(data$ScaledGap_1m)] <- 0

# Turns out that I don't want MeanGap_1m but the percent in gaps greater than or
# equal to 1 m
data$GapCover_101_plus <- mapply(X = data$GapCover_101_200,
                                 Y = data$GapCover_200_plus,
                                 FUN = sum)

#### PLOTTING ##################################################################
# Okay! So what we're after is:
# A box-and-scatterplot combo depicting proposed benchmark categorizations
# (based on the quantiles) with shape/color of the points
# It should be three plots, one for each scale
# Scales to use are MLRA, LRU, and Ecological Site Group (Sandy and Shallow Sandy)
# Indicators to start with are  bare ground, total foliar cover, gaps >1 m, and
# Q (from AERO)
indicators <- c("Bare soil (%)" = "BareSoilCover",
                "Total foliar cover (%)" = "TotalFoliarCover",
                "Area in gaps greater than 1m (%)" = "GapCover_101_plus",
                # "Mean area in gaps greater than 1m" = "MeanGap_1m",
                "Q" = "horizontal_flux_total_MD")

# Get the median per-indicator at the MLRA scale
# This is just easier with the indicators vector already made
data_tall <- tidyr::pivot_longer(data = dplyr::select(.data = data,
                                                      PrimaryKey,
                                                      dplyr::all_of(unname(indicators))),
                                 cols = dplyr::all_of(unname(indicators)),
                                 names_to = "indicator",
                                 values_to = "value")
medians <- dplyr::summarize(dplyr::group_by(.data = data_tall,
                                            indicator),
                            median = median(value))

# We need quantile info for controlling aesthetics reasons
# But quantiles are calcuated per-subset
plotting_data_list <- list(MLRA = data[grep(data$MLRARSYM, pattern = "^42"), ],
                           LRU = data[data$MLRARSYM %in% "42B", ],
                           ESG = data[data$esg %in% "Sandy and Shallow Sandy Ecological Site Group", ])

# Add the group variable to these
plotting_data_wide_list <- lapply(X = names(plotting_data_list),
                                  data_list = plotting_data_list,
                                  FUN = function(X, data_list){
                                    current_data <- data_list[[X]]
                                    current_data$group <- X
                                    current_data
                                  })
plotting_data_wide <- dplyr::bind_rows(plotting_data_wide_list)
plotting_data_wide$group <- factor(x = plotting_data_wide$group,
                                   levels = c("MLRA", "LRU", "ESG"))

# Convert them to long format and get quantile info in there too
plotting_data_long_list <- lapply(X = names(plotting_data_list),
                                  data_list = plotting_data_list,
                                  indicators = indicators,
                                  FUN = function(X, data_list, indicators){
                                    current_data <- data_list[[X]]
                                    
                                    current_data$group <- X
                                    
                                    # OKAY! So for each indicator, we'll
                                    # find the quantile breaks and then use those
                                    # to assign a quantile to each value
                                    quantile_data_list <- lapply(X = names(indicators),
                                                                 indicators = indicators,
                                                                 current_data = current_data,
                                                                 FUN = function(X, indicators, current_data){
                                                                   indicator <- indicators[[X]]
                                                                   
                                                                   # Make the most pared-down data frame possible here
                                                                   current_data <- data.frame(PrimaryKey = current_data$PrimaryKey,
                                                                                              group = current_data$group,
                                                                                              indicator = indicator,
                                                                                              indicator_name = X,
                                                                                              value = current_data[[indicator]])
                                                                   
                                                                   message(paste0("Working on ", indicator))
                                                                   current_vector <- current_data[["value"]]
                                                                   # Get the quantile cutoffs
                                                                   quantile_cutoffs <- quantile(x = current_vector)
                                                                   
                                                                   # We're going to make a vector of each quantile "name"
                                                                   # e.g., 50%-75%
                                                                   # We're also going to assign that to each relevant value
                                                                   quantiles <- c()
                                                                   for (quantile_id in length(quantile_cutoffs):2) {
                                                                     # Get the upper and lower cutoff values
                                                                     upper <- quantile_cutoffs[quantile_id]
                                                                     lower <- quantile_cutoffs[quantile_id - 1]
                                                                     
                                                                     # Determine if a value is below the current upper
                                                                     # bound and above the current lower bound for
                                                                     # the quantile
                                                                     below_upper <- current_vector <= upper
                                                                     above_lower <- current_vector >= lower
                                                                     
                                                                     applicable_indices <- mapply(X = below_upper,
                                                                                                  Y = above_lower,
                                                                                                  FUN = function(X, Y){
                                                                                                    X & Y
                                                                                                  })
                                                                     
                                                                     # Write in the current quantile ID to the relevant indices
                                                                     current_data$quantile_id[applicable_indices] <- quantile_id - 1
                                                                     
                                                                     # Make a name for the quantile, e.g., "50%-75%"
                                                                     quantile <- paste0(names(quantile_cutoffs)[quantile_id - 1],
                                                                                        "-",
                                                                                        names(quantile_cutoffs)[quantile_id])
                                                                     
                                                                     # Write that name into the relevant indices
                                                                     current_data$quantile[applicable_indices] <- quantile
                                                                     
                                                                     # Save the name
                                                                     quantiles <- c(quantiles,
                                                                                    quantile)
                                                                   }
                                                                   
                                                                   # We went from highest value to lowest, but we'd like that reversed
                                                                   # So here' we'll reverse that
                                                                   quantiles <- quantiles[length(quantiles):1]
                                                                   
                                                                   # And then we can use that low-to-high vector to reorder the factors
                                                                   # which'll let us plot these in a reasonable order
                                                                   current_data$quantile <- factor(x = current_data$quantile,
                                                                                                   levels = quantiles)
                                                                   current_data
                                                                 })
                                    dplyr::bind_rows(quantile_data_list)
                                  })
plotting_data_long <- dplyr::bind_rows(plotting_data_long_list)

# So we can order things the way we want in figures!
plotting_data_long$group <- factor(x = plotting_data_long$group,
                                   levels = c("MLRA", "LRU", "ESG"))
plotting_data_long$indicator_name <- factor(x = plotting_data_long$indicator_name,
                                            levels = names(indicators))

# This is going to get complicated so we can control facet and axis labeling
# Basically, we're going to make all the individual plots and then patchwork
# them together.

# First up is to create the table of parameters for each plot
# This'll let us generate them without lots of redundant code and make it easier
# to change the aesthetics of all the plots without lots of CTRL+F and possible
# missed arguments
# This is ordered so that it works with the manual layout below
plot_parameters <- expand.grid(group = c("MLRA",
                                         "LRU",
                                         "ESG"),
                               indicator = c("TotalFoliarCover",
                                             "BareSoilCover",
                                             "GapCover_101_plus"))
plot_parameters$column <- rep(c("left", "middle", "right"),
                              times = 3)
plot_parameters$row <- as.vector(sapply(X = c("top", "middle", "bottom"),
                                        times = 3,
                                        FUN = rep))
plot_parameters$xlim_max[plot_parameters$indicator %in% c("TotalFoliarCover", "BareSoilCover")] <- 100
plot_parameters$xlim_max[plot_parameters$indicator %in% c("MeanGap_1m")] <- 1500

# Create the plots!
plots <- lapply(X = 1:nrow(plot_parameters),
                parameters = plot_parameters,
                plotting_data = plotting_data_long,
                FUN = function(X, parameters, plotting_data){
                  current_data <- dplyr::filter(plotting_data,
                                                group == parameters$group[X],
                                                indicator == parameters$indicator[X])
                  
                  # Wrap the labels so they don't overlap
                  current_data$indicator_name_wrapped <- stringr::str_wrap(string = current_data$indicator_name,
                                                                           width = 23)
                  
                  # Here's the basic plot which we'll modify according to the
                  # parameters provided for the location in the figure
                  plot <- ggplot(data = current_data) +
                    geom_jitter(aes(x = value,
                                    y = indicator_name_wrapped,
                                    color = quantile),
                                shape = 16,
                                size = 0.75,
                                alpha = 0.6) +
                    geom_boxplot(aes(x = value,
                                     y = indicator_name_wrapped),
                                 fill = NA,
                                 outlier.shape = NA) +
                    scale_color_viridis_d(end = 0.8) +
                    xlim(c(0, parameters$xlim_max[X])) +
                    labs(y = "Indicator",
                         x = "Value",
                         color = "Quantile") +
                    facet_grid(cols = vars(group),
                               rows = vars(indicator_name)) +
                    theme(panel.border = element_blank(),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_rect(fill = "white"),
                          axis.line = element_line(colour = "black"),
                          axis.title.y = element_blank(),
                          axis.text.y = element_text(color = "black"),
                          axis.ticks.y = element_blank(),
                          legend.position = "none")
                  
                  # Adjust based on row
                  if (parameters$row[X] == "top") {
                    # Top should have the x-axis facet label and no x-axis title
                    plot <- plot +
                      theme(axis.title.x = element_blank(),
                            strip.text.y = element_blank(),
                            strip.text.x = element_text(face = "bold"),
                            strip.background.x = element_rect(fill = "white"))
                  }
                  
                  if (parameters$row[X] == "bottom") {
                    # Bottom should have no x-axis facet label and an x-axis
                    # label only for the middle plot
                    # We'll handle that special plot last
                    plot <- plot +
                      theme(strip.text.y = element_blank(),
                            strip.text.x = element_blank())
                  }
                  
                  if (parameters$row[X] == "middle") {
                    # Everything in between should have neither label
                    plot <- plot +
                      theme(axis.title.x = element_blank(),
                            strip.text.y = element_blank(),
                            strip.text.x = element_blank())
                  }
                  
                  # And now adjust for column
                  if (parameters$column[X] == "left") {
                    # Only the leftmost column gets the y-axis label
                    plot <- plot +
                      theme(axis.text.y.left = element_text(angle = 90,
                                                            size = 10,
                                                            hjust = 0.5))
                  }
                  
                  if (parameters$column[X] == "right") {
                    plot <- plot +
                      theme(axis.text.y.left = element_blank(),
                            axis.ticks.y = element_blank())
                  }
                  
                  if (parameters$column[X] == "middle") {
                    plot <- plot +
                      theme(axis.text.y.left = element_blank(),
                            axis.ticks.y = element_blank())
                  }
                  
                  # And the special bottom middle plot
                  if (parameters$row[X] == "bottom" & parameters$column[X] != "middle") {
                    plot <- plot +
                      theme(axis.title.x = element_blank())
                  }
                  
                  plot
                })

# And the faceted bare soil versus Q plot
q_vs_baresoil_plot <- ggplot(data = plotting_data_wide) +
  geom_point(aes(y = horizontal_flux_total_MD,
                 x = BareSoilCover)) +
  geom_smooth(aes(x = BareSoilCover,
                  y = horizontal_flux_total_MD),
              color = "darkgray",
              se = FALSE,
              linewidth = 0.5) +
  xlim(c(0, 100)) +
  labs(x = "Bare soil (%)",
       # I *mostly* understand what I'm doing here because it's cobbled together
       # from examples instead of based on actually figuring out what the help
       # docs are saying, but the important thing is that it works
       # This labels the y axis with Q (g m^-1 d^-1) but the Q is italicized and
       # the superscripts are actually superscript
       y = expression(italic(Q)~textstyle(group("(", g~m^{-1}~d^{-1}, ")")))) +
  facet_wrap(facets = ~ group,
             ncol = length(plotting_data_list)) +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        # axis.text.y.left = element_blank(),
        axis.text.x = element_text(size = 10),
        strip.text.y = element_blank(),
        strip.text.x = element_blank())

# Patchwork time!
# This is the layout. Each of the nine boxplots gets placed in the grid
# (the plot_parameters is ordered specifically for this step)
# then the whole bottom row is the Q vs bare soil plot so it's facets align
# with the columns
manual_layout <- c("ABC
                   DEF
                   GHI
                   JJJ")

# Programatically make the string to patchwork all these plots together
patchwork_eval_string <- paste0(paste0("plots[[",
                                                    1:length(plots),
                                                    "]]",
                                                    collapse = " + "),
                                             " + q_vs_baresoil_plot",
                                             " + plot_layout(design = manual_layout, guides = 'collect')")

# Make the combined plot!
combined_plot <- eval(parse(text = patchwork_eval_string))

# Just look at it to see that it makes sense
combined_plot

#### WRITING ###################################################################
ggsave(plot = combined_plot,
       filename = paste0(filepath, "/figures/",
                         "indicator_quantiles_figure_20230828.png"),
       device = "png")

write.csv(x = attribute_lut,
          file = paste0(filepath, "/data/", "attribute_lut.csv"))
