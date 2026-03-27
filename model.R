# Run analysis, write model results

# Before:
# After:

library(icesTAF)

source("utilities.R")

mkdir("model")

# load data
d2all <- read.csv("data/d2all.csv")
d4 <- read.csv("data/d4.csv")

# Group metiers - inventories

d2allg <- d2all %>%
  separate_rows(Gear, sep = "[,;/]\\s*") %>%
  mutate(Gear = str_trim(Gear),
         GearGroup = vapply(Gear, classify_gear, character(1)))
tabgear <- distinct(d2allg, Gear, GearGroup)

saveRDS(tabgear, "model/tabgear.rds")

# Group gears - CE - run at the end its taking too long
# ce <- mutate(ce, GearGroup = vapply(CEmetier6, classify_gear, character(1))) # takes too long
# tabgearce <- distinct(ce, CEmetier6, GearGroup)
# datatable(tabgearce, caption = "Grouped metiers in the old datacall")


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
  mutate(parsed = map(AreaCode_collapsed, parse_one_code)) %>% # the error is in the nested functions above, not here
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


# Fix specific stuff
d2div <- mutate(d2div, AreaCode = ifelse(AreaCode %in% "37.2.1", "GSA 17", AreaCode))
# Exclude 3271 France (talked with Sebastien)
# Exclude 27.3.bc
d2div <- filter(d2div, !(AreaCode %in% "3721" & Country %in% "FR"& Source %in% "2025DC"))
d2div <- filter(d2div, !(AreaCode %in% "27.3.bc" & Country %in% "FR"& Source %in% "2025DC"))
# Reassign 3.a to 4.b (rectangle ovelap issue)
d2div <- mutate(d2div, AreaCode = ifelse(Country %in% "FR" & AreaCode %in% "27.3.a"& Source %in% "2025DC", "27.4.b", AreaCode))

# Exclude Finland area 27.3.d. (all Baltic) - Email for new submission: Done
# d2div <- filter(d2div, !AreaCode %in% "27.3.d.")

# Fix Slovenia
d2div <- mutate(d2div, AreaCode_system = ifelse(Country %in% "SI", "GSA", AreaCode_system))


# Several records in the old datacall don't have area
naArea <- filter(d2div, is.na(AreaCode))
d2div <- filter(d2div, !is.na(AreaCode))


d2div <- d2div %>%
  mutate(AreaCode = ifelse(substr(AreaCode1, 1, 4) == "27.3", substr(AreaCode1, 1, 9), sub("^(([^.]*\\.){2}[^.]*).*", "\\1", AreaCode1)))



med <- filter(d2div, AreaCode_system == "GSA")
# Slovenia is grouped with ICES area codes need to fix: Done
nor <- filter(d2div, !AreaCode_system == "GSA")




current_year <- year(Sys.Date())

# Factors to be plotted in order
dfnor <- nor  %>%
  mutate(AreaCode2 = factor(AreaCode, levels = unique(ices_divisions$division)),
         GearGroup1 = factor(GearGroup, levels = c("DemTrawl", "DemSeine","Gillnetters", "LinesHooks", "PelSeine", "PelTrawl", "Others")))

# Remove NAFO areas
dfnor <- filter(dfnor, !is.na(AreaCode2))

# Determine sampling schemes with logbooks monitoring method
d4mm <- filter(d4, MonitoringMethod %in% "LB")  # Estonia
remLB <- unique(d4mm$SamplingScheme)

# With zeros - NAs are created by expanding the df
# These have been left in - Need to check if they correspond to 0 trips sampled
yearsCellWZ <- dfnor %>%
  filter(Year >= 2017 & Year < 2025) %>% # 2017 - 2024
  filter(!SamplingScheme %in% remLB) %>%
  group_by(AreaCode2, GearGroup1) %>%
  summarise(NoYears = length(unique(Year)))

# Without 0's
yearsCellWTZ <- dfnor %>%
  filter(Year >= 2017 & Year < 2025) %>%
  filter(!SamplingScheme %in% remLB) %>%
  filter(NoTripsSampled != 0) %>%
  filter(is.na(NoTripsSampled) == F) %>%
  group_by(AreaCode2, GearGroup1) %>%
  summarise(NoYears = length(unique(Year)))

saveRDS(yearsCellWTZ, "model/yearsCellWTZ.rds")
