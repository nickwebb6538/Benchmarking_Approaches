# Update ecological site codes, and standardize across LMF and AIM
# Data should be read in and column offset corrected, if needed
# Read in site synonym list from EDIT
es_syns <- read.csv("code\\ecosite_synonyms_list.csv")
BLM_indicators[["ES_stripped"]] <- gsub(BLM_indicators[["EcologicalSiteId"]],
                                        pattern = "^[RF]", replacement = "")

# Strip legacy names in EDIT list
es_syns[["ES_stripped"]] <- gsub(es_syns[["Synonym"]],
                                 pattern = "^[RF]", replacement = "")
# Join indicators df and site names
BLM_indicators <- dplyr::left_join(BLM_indicators, es_syns)

# Determine type 1 or type 2 ecological site
# Extract all sites from dataset
classlist <- as.data.frame(unique(BLM_indicators$Ecological.site.ID))
colnames(classlist) <- "ecoclasses"
classlist <- na.omit(classlist)
# Extract LRU
classlist <- classlist %>%
  dplyr::mutate(geoUnit = ifelse(grepl("^R", classlist$ecoclasses),
                                 substr(classlist$ecoclasses, 2, 5),
                                 ifelse(grepl("^F", classlist$ecoclasses),
                                        substr(classlist$ecoclasses, 2, 5),
                                        ifelse(grepl("^D", classlist$ecoclasses),
                                               substr(classlist$ecoclasses, 3, 6), NA))))
# Seperate list of geounits
geoUnits <- as.data.frame(unique(classlist$geoUnit))
colnames(geoUnits) <- "geoUnit"

# Make a list of all the data frames for the ecoclasses IDs
sp.comp.raw <- lapply(X = unique(geoUnits$geoUnit), FUN = function(X) {
  read.table(paste0("https://edit.jornada.nmsu.edu/services/downloads/esd/", X, "/rangeland-plant-composition.txt"), sep="\t", quote = "\"", fill = TRUE)
})

sp.comp <- dplyr::bind_rows(sp.comp.raw)
# Remove metadata
sp.comp <- sp.comp[-c(1:2), ]
# Make headers column names
names(sp.comp) <- lapply(sp.comp[1, ], as.character)
sp.comp <- sp.comp[-1, ]
names(sp.comp) <- gsub(" ", ".", names(sp.comp))
# Subset to ESs in dataset
sp.comp.ss <- subset(sp.comp, sp.comp$Ecological.site.ID %in% classlist$ecoclasses)

# QC - are all ESs present?
unique(sp.comp.ss$Ecological.site.ID)

sitecount <- classlist %>%
  dplyr::group_by(geoUnit) %>%
  dplyr::summarise(count = n())
sitecount$type <- "classlist"
siteqc <- sp.comp.ss %>%
  dplyr::select(MLRA, Ecological.site.ID) %>%
  dplyr::distinct() %>%
  dplyr::group_by(MLRA) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::rename(geoUnit = MLRA)
siteqc$type <- "edit"
siteqc <- rbind(siteqc, sitecount)
# 042C is missing from EDIT data, and a few from 042B
CL042C <- dplyr::filter(classlist, geoUnit == "042C")
edit042C <- subset(sp.comp, sp.comp$Ecological.site.ID %in% CL042C$ecoclasses)
X <- dplyr::filter(sp.comp, Ecological.site.ID == "R042CY101NM")
X <- subset(es_syns, es_syns$Ecological.site.ID %in% CL042C$ecoclasses)
# The sites listed as 042C are not available in EDIT - their synynomys are under
# 070C and 070D, which are not in EDIT
# Some missing after filtering sp.comp for sites present in dataset
unique(BLM_indicators$"Ecological.site.ID")
unique(classlist$ecoclasses)
# One less in ecoclasses, because NA is removed
x <- subset(classlist, !(classlist$ecoclasses %in% sp.comp.ss$`Ecological site ID`))
y <- subset(es_syns, es_syns$Ecological.site.ID %in% x$ecoclasses)


# Assign site type based on FG data
unique(sp.comp.ss$Plant.type)
# Subset data to reference state
names(sp.comp.ss)
unique(sp.comp.ss$Ecosystem.state)
# Production columns should be numeric
sp.comp.ss$Production.low <- as.numeric(sp.comp.ss$Production.low)
sp.comp.ss$Production.high <- as.numeric(sp.comp.ss$Production.high)
sp.comp.grass.sums <- sp.comp.ss %>%
  dplyr::filter(Ecosystem.state == "1") %>%
  dplyr::filter(Plant.type == "grass/grasslike") %>%
  dplyr::group_by(Ecological.site.ID) %>%
  dplyr::summarise(APGrassLow = sum(Production.low),
                   APGrassHigh = sum(Production.high))
sp.comp.shrub.sums <- sp.comp.ss %>%
  dplyr::filter(Ecosystem.state == "1") %>%
  dplyr::filter(Plant.type == "shrub/vine") %>%
  dplyr::group_by(Ecological.site.ID) %>%
  dplyr::summarise(APShrubLow = sum(Production.low),
                   APShrubHigh = sum(Production.high))

sp.comp.sums <- dplyr::left_join(sp.comp.grass.sums, sp.comp.shrub.sums)


sp.comp.sums <- sp.comp.sums %>%
  dplyr::mutate(Type = ifelse(Ecological.site.ID == "R042BB010NM" |
                         Ecological.site.ID == "R042BB011NM" |
                         Ecological.site.ID == "R042BB035NM" |
                         Ecological.site.ID == "R042BB024NM" |
                         Ecological.site.ID == "R042BB027NM" |
                         Ecological.site.ID == "R042BB021NM" |
                         Ecological.site.ID == "R042BB013NM" |
                         Ecological.site.ID == "R042BB036NM" |
                         Ecological.site.ID == "R042BB037NM", 2,
                        ifelse(Ecological.site.ID == "R042BB018NM" |
                                 Ecological.site.ID == "R042BB028NM" |
                                 Ecological.site.ID == "R042BB016NM" |
                                 Ecological.site.ID == "R042BB012NM" |
                                 Ecological.site.ID == "R042BB015NM" |
                                 Ecological.site.ID == "R042BB019NM" |
                                 Ecological.site.ID == "R042BB014NM" |
                                 Ecological.site.ID == "R042BB023NM" |
                                 Ecological.site.ID == "R042BB006NM", 1, NA)))

sp.comp.sums <- sp.comp.sums %>%
  dplyr::mutate(RatioLow = round(APGrassLow/APShrubLow, 0),
                RatioHigh = round(APGrassHigh/APShrubHigh, 0))

# Find range in ratios for site types
ratiorange <- sp.comp.sums %>%
  dplyr::group_by(Type) %>%
  dplyr::summarise(MinLow = min(RatioLow),
                   MaxLow = max(RatioLow),
                   MinHigh = min(RatioHigh),
                   MaxHigh= max(RatioHigh))

# Apply to unspecified sites
sp.comp.sums <- sp.comp.sums %>%
  dplyr::mutate(Type = ifelse(is.na(Type) & RatioLow < 6, 2,
                ifelse(is.na(Type) & RatioLow > 5, 1, Type)))
