library(TAF)

taf.skeleton()

# development cycle

# create data entry in DATA.bib
# first time
draft.data(file = TRUE)

# populate data folder
taf.boot()

# next time
draft.data(file = FALSE)

# to run each section:
source.taf("data")
source.taf("model")
source.taf("report")


