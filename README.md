# WindErosionBenchmarks
This repository contains data and code for different approaches to establish quantitative benchmarks for soil erosion and ecological monitoring, assessment and management.

# Authors

Nicholas P. Webb1, Brandon L. Edwards1, Alexandra Heller1, Sarah E. McCord1, Jeremy W. Schallner1, Ronald S. Treminio1, Brandi Wheeler1, Nelson G. Stauffer1, Sheri Spiegal1, Michael C. Duniway2, Alexander C.E. Traynor3, Emily Kachergis3, Carrie-Ann Houdeshell4

1 USDA-ARS Jornada Experimental Range, Las Cruces, NM, USA
2 US Geological Survey, Southwest Biological Science Center, Moab, UT, USA
3 Bureau of Land Management, National Operations Center, Lakewood, CO, USA
4 USDA-NRCS Resource Inventory and Assessment Division, CEAP-Grazing Lands, Davis, CA, USA

# Created

2011-2019

# Purpose

This repository contains data and R code used and generated by Webb et al. for the manuscript "Establishing quantitative benchmarks for soil erosion and ecological monitoring, assessment and management" submitted for publication in Ecological Indicators.

# Data

Monitoring data were collected in the northern Chihuahuan Desert by the US Bureau of Land Management’s (BLM) Assessment, Inventory and Monitoring (AIM) program (Toevs et al., 2011). Data were subset to the extent of Major Land Resource Area (MLRA) 42 – the Southern Desertic Basins, Plains and Mountains. The benchmarking approaches we present span spatial scales from the MLRA to Land Resource Unit (LRU), ecological site group (ESG) and ecological sites.

AIM data were collected using the standard methods of Herrick et al. (2018), including line-point intercept, canopy gap intercept, and vegetation height. Data were collected at 678 monitoring plots sampled between 2011 and 2019 using stratified random sample designs. Ground cover indicators of wind erosion were calculated from the monitoring data using the terradactyl R package (McCord et al., 2022), including total foliar cover (%), bare soil (%), cover of canopy gaps >100 cm (%), vegetation height (m), and mean scaled gap size – calculated as mean canopy gap size divided by mean vegetation height. 

Estimates of horizontal aeolian sediment mass flux, Q (g m-1 d-1) were produced for each BLM AIM monitoring plot with the Aeolian EROsion (AERO) model. AERO was parameterized using Generalized Likelihood Uncertainty Estimation (GLUE) (Edwards et al., 2022). All data used here, including model estimates, are also publicly available through the Landscape Data Commons (https://landscapedatacommons.org). 

Analyses and graphs to illustrate benchmark establishment were developed using R version 4.2.1.


# Repository includes:

1.	Data (folder): aero.csv, BLM_indicators.csv, EDIT_ublic_ecological_site_list.csv, indicators.csv, indicators_2022-10-22.csv, sandy_esg_data.csv, sandy_state_all.csv, gap_tall.Rdata, header.Rdata, height_tall.Rdata, lpi_tall.Rdata, soil_stability_tall.Rdata, species_inventory_tall.Rdata, summary_v2_AIM_FLUX_OUTPUT.csv
2.	Data_Extraction (folder): data_subsetting.csv, indicator_calcs.R, indicators_calcs_2022-10-21.R
3.	Fuzzy_Clustering (folder): ecosite_synonyms_list.csv, high.mems.csv, high.sums.csv, missingsites.csv, topmem.summary.csv, topmems.csv, assigning_sitetype.R, cluster_plotting.R, fuzzy_clustering.R, class-synonym-list.txt
4.  Linear_segmented_regression_variance_approaches.R
5.  Application_ecological_site_and_state_Concepts.R
6.  Quantiles.R
7.  Indicator_quantiles_by_scale_20230825.R	

