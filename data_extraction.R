# Test data
# data.path <- "~/Desktop/Godley lab/qPCR data/20190109 JQ1 Hypoxia Plate 1_data.xls"

# function for reading excel qPCR data

extract.data <- function(path){
  library(readxl)
  # read entire table
  raw.data <- read_excel(path, col_names = F)
  # determine header rows and re-read table without the header rows
  nrow.header <- which(grepl(TRUE,is.na(raw.data[,1])))
  raw.data <- read_excel(path,skip = nrow.header)
  
  # extract essential columns
  essential.columns <- c("Sample Name","Target Name","Task","Cт","Quantity")
  raw.data <- raw.data[,essential.columns]
  
  # remove empty rows
  empty.rows <- apply(raw.data, 1, function(raw.data) all(is.na(raw.data)))
  raw.data <- raw.data[!empty.rows,]
  
  # reformat certain colomns
  raw.data$`Sample Name` <- as.factor(raw.data$`Sample Name`)
  raw.data$`Target Name` <- as.factor(raw.data$`Target Name`)
  raw.data$Task <- as.factor(raw.data$Task)
  
  # separate data by tasks
  tasks <- levels(raw.data$Task)
  sep.data <- lapply(tasks,function(x) raw.data[raw.data$Task==x,])
  names(sep.data) <- tasks
  return(sep.data)
}



