library(tidyr)
library(terradactyl)
library(sf)

# get MLRA 2022 boundaries
mlra <- sf::st_read("data/MLRA_52_2022/MLRA_52.shp")
mlra_42 <- mlra %>% subset(MLRARSYM %in% c("42A", "42B", "42C"))

# get AIM data, constrained by plots we have AERO estimates for
aero <- read.csv("data/raw/summary_v2_AIM_FLUX_OUTPUT.csv")

header <- readRDS("~/Projects/AIM/Data/AIM_tall_tables_export_2022-05-11/header.Rdata") %>%
  subset(PrimaryKey %in% aero$PrimaryKey)

# subset to MLRA 42
header <- header %>% sf::st_as_sf(coords = c("Longitude_NAD83", "Latitude_NAD83"),
                                  crs = 4269) %>% sf::st_set_crs(4269) %>%
  # transform
  sf::st_transform(crs = st_crs(mlra_42))

# subset to MLRA 42
header_42 <- sf::st_join(header, mlra_42, join = sf::st_within) %>%
  subset(MLRARSYM %in% c("42A", "42B", "42C"))


plot(st_geometry(mlra_42), axes = TRUE, graticule = TRUE)
plot(header_42['SpeciesState'], axes = TRUE, graticule = TRUE, pch = 16, add = TRUE)

saveRDS(header_42, "data/raw/header.Rdata")

# save AERO data from in MLRA 42
write.csv(aero %>% subset(PrimaryKey %in% header_42$PrimaryKey),
        "data/indicators/aero.csv")

# read and save LPI data
lpi <- readRDS("~/Projects/AIM/Data/AIM_tall_tables_export_2022-05-11/lpi_tall.Rdata") %>%
  subset(PrimaryKey %in% header_42$PrimaryKey)
saveRDS(lpi, "data/raw/lpi_tall.Rdata")

# read and save height data
height <- readRDS("~/Projects/AIM/Data/AIM_tall_tables_export_2022-05-11/height_tall.Rdata") %>%
  subset(PrimaryKey %in% header_42$PrimaryKey)
saveRDS(height, "data/raw/height_tall.Rdata")

# read and save gap data
gap <- readRDS("~/Projects/AIM/Data/AIM_tall_tables_export_2022-05-11/gap_tall.Rdata") %>%
  subset(PrimaryKey %in% header_42$PrimaryKey)
saveRDS(gap, "data/raw/gap_tall.Rdata")

# read and save species inventory data
spp_inventory <- readRDS("~/Projects/AIM/Data/AIM_tall_tables_export_2022-05-11/species_inventory_tall.Rdata") %>%
  subset(PrimaryKey %in% header_42$PrimaryKey)
saveRDS(spp_inventory, "data/raw/species_inventory_tall.Rdata")

# read and save soil stability data
soil_stab <- readRDS("~/Projects/AIM/Data/AIM_tall_tables_export_2022-05-11/soil_stability_tall.Rdata") %>%
  subset(PrimaryKey %in% header_42$PrimaryKey)
saveRDS(soil_stab, "data/raw/soil_stability_tall.Rdata")
