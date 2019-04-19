library(readxl)
# read entire table
# data.path <- "~/Desktop/Godley lab/qPCR data/20180712 ChIP qPCR validation_data.xls"
data.path <- "~/Desktop/Godley lab/qPCR data/20190109 JQ1 Hypoxia Plate 1_data.xls"
data <- read_excel(data.path,
                   col_names = F)
# determine header rows and re-read table without the header rows
nrow.header <- which(grepl(TRUE,is.na(data[,1])))
data <- read_excel(data.path,skip = nrow.header)

# reformat certain colomns
data$`Sample Name` <- as.factor(data$`Sample Name`)
data$`Target Name` <- as.factor(data$`Target Name`)
data$Task <- as.factor(data$Task)

# Check types of tasks

include.standard <- "STANDARD" %in% levels(data$Task)
if(!"NTC" %in% levels(data$Task)){
  warning("No NTC found!")
}

# clean up unessessary rows and columns


levels(data$`Sample Name`)

