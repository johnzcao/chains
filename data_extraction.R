# Test data
#data.path <- c("Test_data_1.xls","Test_data_2.xls")
#data.path <- c("Test_data_1.xls")

extract.data <- function(path){
  library(readxl)
  # read entire table
  raw.data <- read_excel(path, col_names = F)
  # determine header rows and re-read table without the header rows
  nrow.header <- which(grepl(TRUE,is.na(raw.data[,1])))
  raw.data <- read_excel(path,skip = nrow.header)
  
  # extract essential columns
  if("CT" %in% colnames(raw.data)){
    colnames(raw.data)[colnames(raw.data)=="CT"] <- "Cт"
  }
  essential.columns <- c("Well","Sample Name","Target Name","Task","Cт","Quantity")
  raw.data <- raw.data[,essential.columns]

  # Add error message if required columns not found
  
  # remove empty rows
  empty.rows <- apply(raw.data, 1, function(raw.data) is.na(raw.data["Task"]))
  raw.data <- raw.data[!empty.rows,]
  
  # reformat certain colomns
  raw.data$Well <- as.character(raw.data$Well)
  raw.data$`Sample Name` <- as.factor(raw.data$`Sample Name`)
  raw.data$`Target Name` <- as.factor(raw.data$`Target Name`)
  raw.data$Task <- as.factor(raw.data$Task)
  raw.data$Cт <- as.numeric(raw.data$Cт)
  raw.data$Quantity <- as.numeric(raw.data$Quantity)
  
  return(raw.data)
}

output <- do.call(rbind,lapply(data.path, function(x) extract.data(x)))

# separate data by tasks
tasks <- levels(output$Task)
sep.data <- lapply(tasks,function(x) output[output$Task==x,])
names(sep.data) <- tasks

# remove intermediate data from environment
rm(output,tasks,extract.data)


