library(terradactyl)

# read in raw data
lpi <- readRDS("data/raw/lpi_tall.Rdata")
gap <- readRDS("data/raw/gap_tall.Rdata")
height <- readRDS("data/raw/height_tall.Rdata")
soil_stab <- readRDS("data/raw/soil_stability_tall.Rdata")


# gaps > 100 m, >200 m
gap_cover <- gap_cover(gap_tall = gap)$percent %>% dplyr::select(PrimaryKey,
              GapCover_101_200 = "101-201",
              GapCover_200_plus = "201-Inf")

# bare soil
bare_soil <- pct_cover_bare_soil(lpi)
bare_soil <- bare_soil %>% dplyr::mutate(BareSoilCover = LM + S)

# total foliar cover
total_foliar <- pct_cover_total_foliar(lpi)

# max height
max_height <-  max_height <- mean_height(
  height_tall = height,
  method = "max",
  omit_zero = TRUE,
  by_line = FALSE,
  tall = TRUE
) %>% dplyr::rename("MeanMaxHeight" = "max_height")

# scaled gap, defined as mean gap/mean height
mean_gap <- gap %>% dplyr::group_by(PrimaryKey) %>%
  dplyr::summarise(MeanGap = mean(Gap))
scaled_gap <- dplyr::left_join(mean_gap, max_height) %>%
  dplyr::mutate(ScaledGap = MeanGap/MeanMaxHeight)

# scaled gap > 1 m
mean_gaps_1m <- gap %>% subset (Gap >=100) %>%
  dplyr::group_by(PrimaryKey) %>%
  dplyr::summarise(MeanGap_1m = mean(Gap))
scaled_gap_1m  <- dplyr::left_join(mean_gaps_1m, max_height) %>%
  dplyr::mutate(ScaledGap_1m = MeanGap_1m/MeanMaxHeight)

# read in BLM Indicators tables
terradat <- sf::st_read("~/Projects/AIM/Data/2021 AIM Terrestrial TerrADat and LMF.gdb/AIMTerrestrialEdtBackup9-1-22.gdb",
                        "TerrADat")
lmf <- sf::st_read("~/Projects/AIM/Data/2021 AIM Terrestrial TerrADat and LMF.gdb/AIMTerrestrialEdtBackup9-1-22.gdb",
                        "LMF")
indicators <- dplyr::bind_rows(terradat, lmf) %>% subset(PrimaryKey %in% lpi$PrimaryKey)

write.csv(indicators, "data/indicators/BLM_indicators.csv")

# build joined method indicators
indicators <- dplyr::left_join(gap_cover, bare_soil %>% dplyr::select(PrimaryKey, BareSoilCover)) %>%
  dplyr::left_join(total_foliar) %>%
  dplyr::left_join(scaled_gap) %>%
  dplyr::left_join(scaled_gap_1m)

write.csv(indicators, "data/indicators/indicators_2022-10-22.csv")
