# Run analysis, write model results

# Before: d2all.csv, d4.csv
# After: d4_importance.rds, d4e.rds, exclusion.rds, hist.rds,
#         tabgear.rds, yearsCellWTZ_GFCM.rds, yearsCellWTZ_ICES.rds

library(icesTAF)

source("utilities.R")

mkdir("model")

# load data
d2all <- read.csv("data/d2all.csv")
d4 <- read.csv("data/d4.csv")

# Document gear groupings -------------------------------------------------

# Group metiers into gear groupings
d2allg <- d2all %>%
  separate_rows(Gear, sep = "[,;/]\\s*") %>%
  mutate(Gear = str_trim(Gear),
         GearGroup = vapply(Gear, classify_gear, character(1)))
tabgear <- distinct(d2allg, Gear, GearGroup)

saveRDS(tabgear, "model/tabgear.rds")

# Calculate range, start year of monitoring programs (Figs 1 & 2) --------------------------

#use only data from this year's call
d4e <- d4 %>%
  mutate(End_year = replace(End_year, is.na(End_year) == T,2025)) %>%
  mutate(End_year = replace(End_year, End_year == 2026 | End_year == 2027, 2025)) %>%
  mutate(range = End_year - Start_year) %>%
  mutate(Start_year = replace(Start_year, Start_year <2017, 2016)) %>%
  mutate(range = End_year - Start_year +1) %>%
  mutate(Start_year = replace(Start_year, Start_year <2017, "<2017"))

saveRDS(d4e, "model/d4e.rds")

# Analyze years of coverage by area by gear group (Figs 3 & 4) -----------------------------

# Collapse list cols, keep original row
d2e <- d2allg %>%
  ungroup() %>%
  mutate(.orig_row = row_number()) %>%
  rowwise() %>%
  mutate(
    AreaCode_collapsed = paste(AreaCode, collapse = "~")
  ) %>%
  ungroup()

# Expand general
d2_expanded <- d2e %>%
select(-AreaCode) %>%
  mutate(parsed = map(AreaCode_collapsed, parse_one_code)) %>% 
  unnest(parsed) %>%
  # clean up
  select(-AreaCode_collapsed)

# Expand subarea -> subdiv
d2div <- d2_expanded %>%
  mutate(
    AreaCode = str_trim(AreaCode)   # just in case there are stray spaces
  ) %>%
  left_join(ices_divisions, by = c("AreaCode" = "subarea")) %>%
  mutate(
    # if AreaCode matches a subarea in the lookup, use that division,
    # otherwise keep the original AreaCode
    AreaCode1 = if_else(!is.na(division), division, AreaCode)
  ) %>%
  select(-division) %>%
  distinct(.orig_row, AreaCode1, AreaCode_system, .keep_all = TRUE)


# Fix specific area assignment issues
d2div <- mutate(d2div, AreaCode = ifelse(AreaCode %in% "37.2.1", "GSA 17", AreaCode))
# Exclude 3271 France
# Exclude 27.3.bc
d2div <- filter(d2div, !(AreaCode %in% "3721" & Country %in% "FR"& Source %in% "2025DC"))
d2div <- filter(d2div, !(AreaCode %in% "27.3.bc" & Country %in% "FR"& Source %in% "2025DC"))
# Reassign 3.a to 4.b (rectangle overlap issue)
d2div <- mutate(d2div, AreaCode = ifelse(Country %in% "FR" & AreaCode %in% "27.3.a"& Source %in% "2025DC", "27.4.b", AreaCode))

# Fix Slovenia -- should be GSA not ICES
d2div <- mutate(d2div, AreaCode_system = ifelse(Country %in% "SI", "GSA", AreaCode_system))


# Several records in the old datacall don't have area
naArea <- filter(d2div, is.na(AreaCode))
d2div <- filter(d2div, !is.na(AreaCode))


d2div <- d2div %>%
  mutate(AreaCode = ifelse(substr(AreaCode1, 1, 4) == "27.3", substr(AreaCode1, 1, 9), sub("^(([^.]*\\.){2}[^.]*).*", "\\1", AreaCode1)))

#separate Med areas from ICES ones
med <- filter(d2div, AreaCode_system == "GSA")
nor <- filter(d2div, !AreaCode_system == "GSA")

current_year <- year(Sys.Date())

## ICES areas --------------------------------------------------------------

# Factors to be plotted in order
dfnor <- nor  %>%
  mutate(AreaCode2 = factor(AreaCode, levels = unique(ices_divisions$division)),
         GearGroup1 = factor(GearGroup, levels = c("DemTrawl", "DemSeine","Gillnetters", "LinesHooks", "PelSeine", "PelTrawl", "Others")))

# Remove NAFO areas
dfnor <- filter(dfnor, !is.na(AreaCode2))

# Determine sampling schemes with logbooks monitoring method
d4mm <- filter(d4, MonitoringMethod %in% "LB")  # Estonia
remLB <- unique(d4mm$SamplingScheme)

# Exclude programs with 0 sampled trips
yearsCellWTZ_ICES <- dfnor %>%
  filter(Year >= 2017 & Year < 2025) %>%
  filter(!SamplingScheme %in% remLB) %>%
  filter(NoTripsSampled != 0) %>%
  filter(is.na(NoTripsSampled) == F) %>%
  group_by(AreaCode2, GearGroup1) %>%
  summarise(NoYears = length(unique(Year)))

saveRDS(yearsCellWTZ_ICES, "model/yearsCellWTZ_ICES.rds")


## GFCM areas --------------------------------------------------------------
# Factors to be plotted in order
dfmed<- med  %>%
  mutate(AreaCode2 = factor(AreaCode, levels = paste0("GSA ", seq(1:30))),
         GearGroup1 = factor(GearGroup, levels = c("DemTrawl", "DemSeine","Gillnetters", "LinesHooks", "PelSeine", "PelTrawl", "Others")))

# Remove NAFO areas
dfmed <- filter(dfmed, !is.na(AreaCode2))

# Without 0's
yearsCellWTZ_GFCM <- dfmed %>%
  filter(Year >= 2017 & Year < 2025) %>%
  filter(!SamplingScheme %in% remLB) %>%
  filter(is.na(GearGroup1) == F) %>%
  filter(NoTripsSampled != 0) %>%
  filter(is.na(NoTripsSampled) == F) %>%
  group_by(AreaCode2, GearGroup1) %>%
  summarise(NoYears = length(unique(Year)))

saveRDS(yearsCellWTZ_GFCM, "model/yearsCellWTZ_GFCM.rds")






# Compare perceived importance (Fig 6) ------------------------------------

#classify by either CF or IB
d4e <- d4e %>% mutate(SST = substrRight(SamplingSchemeType,2))

d4_importance <- d4e %>% group_by(SST) %>%
  mutate(Mammals = mean(ImportanceMammals), Birds = mean(ImportanceBirds), Fish = mean(ImportancePETfish), Elasmobranchs = mean(ImportanceElasm), Reptiles = mean(ImportanceRept)) %>%
  dplyr::select(SST, Mammals, Birds, Fish, Elasmobranchs, Reptiles) %>% distinct()

d4_importance <- pivot_longer(d4_importance, !SST, names_to = "Taxon", values_to = "Importance")

saveRDS(d4_importance, "model/d4_importance.rds")

# Tabulate exclusion criteria (Fig 7)-------------------------------------------------------------------------
exclusion <- d4e %>% filter(ExclusionFlag == "Y") %>%
  filter(is.na(ExclusionCriteria) == F) %>%
  mutate(ExclusionCriteria = replace(ExclusionCriteria, ExclusionCriteria == "low fishing effort", "Low fishing effort")) %>%
  mutate(ExclusionCriteria = replace(ExclusionCriteria, ExclusionCriteria == "Vessel space limitation~Uncooperative vessel", "Uncooperative vessel~Vessel space limitation")) %>%
  mutate(ExclusionCriteria = replace(ExclusionCriteria, ExclusionCriteria == "Small vessel size~Vessel space limitation~Low fishing effort", "Small vessel size~Low fishing effort~Vessel space limitation"))

saveRDS(exclusion, "model/exclusion.rds")


# Calculate monitoring coverage (Fig 8) -----------------------------------

#exclude logbook programs
lb <- d4 %>% filter(MonitoringMethod == "LB")
#look at monitoring coverage relative to thresholds
hist <- d2 %>% mutate(prop = NoTripsSampled / NoTripsTotal) %>%
  filter(!SamplingScheme %in% lb$SamplingScheme) %>%
  filter(prop > 0 & prop <1)

saveRDS(hist, "model/hist.rds")

# Other statistics --------------------------------------------------------
#percentage of programs with scientific observers
so <- length(which(d4e$MonitoringMethod %in% c("SO","SO~PO","SO~VO")))
so/length(d4e$MonitoringMethod)

#number of countries utilizing EM
em <- d4e %>% filter(MonitoringMethod == "EM") %>%
  select(Country) %>%
  distinct()

#proportion that sample 'All' and 'Protected Species'
length(which(d4$SamplingProtocol == "All"))/length(d4$SamplingProtocol)
length(which(d4$SamplingProtocol == "ProtectedSpecies"))/length(d4$SamplingProtocol)

#how many exclusions relate to uncooperative
length(which(exclusion$ExclusionCriteria %in% c("Uncooperative vessel", "Uncooperative vessel~Vessel space limitation", "Uncooperative vessel~Small vessel size")))

#what % of schemes are below 10% threshold
length(which(hist$prop < 0.1)) / length(hist$prop)

#what % of schemes are below 0.1% threshold
length(which(hist$prop < 0.001)) / length(hist$prop)

#what % of schemes are below 10% threshold
length(which(hist$prop < 0.01)) / length(hist$prop)

#part of fishing operation observed
part_observed <- d4 %>% group_by(PartObserved) %>% tally()
length(which(grepl("So", d4$PartObserved) == T))