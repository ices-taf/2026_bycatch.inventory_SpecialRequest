# Produce plots and tables for report

# Before: 
# After: report.html

library(icesTAF)

mkdir("report")

rmarkdown::render("report.Rmd", output_dir = "report")
