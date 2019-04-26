# Main script for data processing

# Test data set(s)
test.data <- c("~/Desktop/Godley lab/qPCR data/20190109 JQ1 Hypoxia Plate 1_data.xls")

# load necessary functions
source("data_extraction.R")
source("data_partition.R")

# data extraction and initial processing
# currently not able to load multiple files
raw.data <- extract.data(path = test.data)
if(!"NTC" %in% levels(raw.data$Task)){
  warning("No NTC found!")
}
tasks <- levels(raw.data$Task)
