# Prepare data, write CSV data tables

# Before:
# After:

library(icesTAF)
library(dplyr)
library(readxl)

mkdir("data")
# READ data

# New datacall
path <- "boot/data/"
d2 <- read.csv(paste0(path, "D2.csv"))
# Need to filter to avoid overlap
d2 <- filter(d2, Year > 2020)
d4 <- read.csv(paste0(path, "D4.csv"))


# Old datacall
d2old <- read_xlsx(paste0(path, "eu.2022.04b_dataset.xlsx"), sheet = "Coverage")
d2old <- filter(d2old, Year >= 2017 & Year <= 2020)
## Remove first row -
d2old <- d2old[-1, ]
## Rename colnames
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
d4old <- d4old[-1, ]


# Bind the d2
d2 <- mutate(d2, Source = "2025DC")
d2old <- mutate(d2old,
                NoTripsSampled = as.numeric(NoTripsSampled),
                NoTripsTotal = as.numeric(NoTripsTotal),
                Source = "2020DC")
d2all<- bind_rows(d2, d2old)

# mutate PT-20 to PT and the two codes for Germany
d2all <- mutate(d2all, Country = ifelse(Country %in% "PT-20", "PT",
                                        ifelse(Country %in% "DEU", "DE", Country)))

# write out
write.taf(c("d2all", "d4"), dir = "data", quote = TRUE)
