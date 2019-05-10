# Test data
#data.path <- c("Test_data_1.xls","Test_data_2.xls")
#data.path <- c("Test_data_1.xls")
#data.path <- c("TF-1 data.xlsx")
#data.path <- c("HL-60 data.xlsx")


extract.data <- function(path){
  library(readxl)
  # read entire table
  raw.data <- read_excel(path, col_names = F)
  # determine header rows and re-read table without the header rows
  header.row <- grepl(TRUE,raw.data[,grep("Sample Name",raw.data)]=="Sample Name") %>% which()
  raw.data <- read_excel(path,skip = header.row-1)
  
  # extract essential columns
  if("CT" %in% colnames(raw.data)){
    colnames(raw.data)[colnames(raw.data)=="CT"] <- "Cт"
  }
  essential.columns <- c("Sample Name","Target Name","Task","Cт")
  optional.columns <- c("Well","Quantity")
  raw.data.reduced <- raw.data[,essential.columns]
  
  for (i in optional.columns){
    if(i %in% colnames(raw.data)){
      raw.data.reduced[,i] <- raw.data[,i]
    }
  }

  # Add error message if required columns not found
  
  # remove empty rows
  empty.rows <- apply(raw.data.reduced, 1, function(raw.data.reduced) is.na(raw.data.reduced["Task"]))
  raw.data.reduced <- raw.data.reduced[!empty.rows,]
  
  # reformat certain colomns
  if("Well" %in% colnames(raw.data)){
    raw.data.reduced$Well <- as.character(raw.data.reduced$Well)
  }
  if("Quantity" %in% colnames(raw.data)){
    raw.data.reduced$Quantity <- as.numeric(raw.data.reduced$Quantity)
  }
  raw.data.reduced$`Sample Name` <- as.factor(raw.data.reduced$`Sample Name`)
  raw.data.reduced$`Target Name` <- as.factor(raw.data.reduced$`Target Name`)
  raw.data.reduced$Task <- as.factor(raw.data.reduced$Task)
  raw.data.reduced$Cт <- as.numeric(raw.data.reduced$Cт)
  
  
  return(raw.data.reduced)
}

output <- do.call(rbind,lapply(data.path, function(x) extract.data(x)))

# separate data by tasks
tasks <- levels(output$Task)
sep.data <- lapply(tasks,function(x) output[output$Task==x,])
names(sep.data) <- tasks

# remove intermediate data from environment
rm(output,tasks,extract.data)


