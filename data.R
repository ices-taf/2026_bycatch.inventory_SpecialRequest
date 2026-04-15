# Prepare data, write CSV data tables

# Before: D2.csv, D4.csv, eu.2022.04b_dataset.xlsx
# After: d2all.csv, d4.csv

library(icesTAF)
library(dplyr)
library(readxl)

mkdir("data")

# Read data ---------------------------------------------------------------

# Read in data from new data call (2025)
# D2 = all programs (from all time)
# D4 = 2022-25 program details, including effort
path <- "boot/data/"
d2 <- read.csv(paste0(path, "D2.csv"))
# Need to filter to avoid overlap with data from previous data call
d2 <- filter(d2, Year > 2020)
d4 <- read.csv(paste0(path, "D4.csv"))


# Read in data from old data call (2022)
d2old <- read_xlsx(paste0(path, "eu.2022.04b_dataset.xlsx"), sheet = "Coverage")
d2old <- filter(d2old, Year >= 2017 & Year <= 2020)

## Rename columns
names(d2old) <- c(
  "Country",
  "Year",
  "SamplingScheme",
  "Gear",
  "AreaCode",
  "NoTripsSampled",
  "NoTripsTotal",
  "SamplingIntensity",
  "ImportanceBirds",
  "ImportanceMammals",
  "ImportancePetFish",
  "ImportanceElasmobranchs",
  "ImportanceReptiles",
  "Comments"
)

d4old <- read_xlsx(paste0(path, "eu.2022.04b_dataset.xlsx"), sheet = "Sample schemes bycatch studies")
#remove first row = rownames
d4old <- d4old[-1, ]

# Bind the d2 (2022 and 2025)
d2 <- mutate(d2, Source = "2025DC")
d2old <- mutate(d2old,
                NoTripsSampled = as.numeric(NoTripsSampled),
                NoTripsTotal = as.numeric(NoTripsTotal),
                Source = "2020DC")
d2all<- bind_rows(d2, d2old)

# mutate PT-20 to PT and unite the two codes for Germany
d2all <- mutate(d2all, Country = ifelse(Country %in% "PT-20", "PT",
                                        ifelse(Country %in% "DEU", "DE", Country)))

# write out
write.taf(c("d2all", "d4"), dir = "data", quote = TRUE)
