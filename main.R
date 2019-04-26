# Main script for data processing

# Test data set(s)
test.data <- c("~/Desktop/Godley lab/qPCR data/20170926 TF-1 TET3 18S_data.xls")

# load necessary functions
source("data_extraction.R")

# data extraction and initial processing
# currently not able to load multiple files
sep.data <- extract.data(path = test.data)
if(!"NTC" %in% names(sep.data)){
  warning("No NTC found!")
}

