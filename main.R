# Main script for data processing

# Test data set(s)
data.path <- c("Test_data_1.xls","Test_data_2.xls")
data.path <- c("Test_data_3.xls")

library(dplyr)
# Define analysis mode and controls. May be interactive in future versions
analysis.method <- "DD" # Plan to have: DD = ΔΔCт, D = ΔCт, chip = percent input, Q = quantity
E <- 2 # Default amplification efficiency unless corrected by standards
hkg <- "18S" # Housekeeping gene
ctr <- "HEL D1" # control sample to compare to
auto.outlier <- TRUE # auto detect and exclude outliers if at least triplicates

# data extraction and initial processing
source("data_extraction.R")

# load auto outlier if enabled
if(auto.outlier==T){
  source("outlier.R")
}

# Warning if no NTC is found in data, but proceed with analysis
if(!"NTC" %in% names(sep.data)){
  warning("No NTC found!")
}

# determine if Standards are present, and calculate mean and SD
if("STANDARD" %in% names(sep.data)){
  # create empty data frames for mean and sd
  standard <- sep.data[["STANDARD"]]
  targets <- levels(standard$`Target Name`)
  dilutions <- levels(as.factor(standard$`Quantity`))
  standard.mean <- data.frame(matrix(NA,nrow = length(dilutions),ncol = length(targets)+1))
  row.names(standard.mean) <- dilutions
  colnames(standard.mean) <- c("Quantity",targets)
  standard.mean$Quantity <- as.numeric(dilutions)
  standard.sd <- standard.mean # copy empty dataframe
  # calculate mean and sd for each target/dilution pair
  # functional but probably inefficient solution with for loops
  for(x in as.numeric(dilutions)){
    for(y in targets){
      z <- standard[standard$Quantity==x & standard$`Target Name`==y,]$Cт
      standard.mean[as.character(x),y] <- mean(z)
      standard.sd[as.character(x),y] <- sd(z)
    }
  }
  # run auto outlier detection
  if(length(z)>2 & auto.outlier==T){
    outlier.report <- outlier(standard,standard.mean,standard.sd,is.standard = T)
  }
  # clean up environment
  rm(dilutions,targets,standard,x,y,z)
}

# Calculate unknown meansand sd 
if(!"UNKNOWN" %in% names(sep.data)) {
  stop("No Unknown found in data!")
}else{
  unknown <- sep.data[["UNKNOWN"]]
  targets <- levels(unknown$`Target Name`)
  samples <- levels(unknown$`Sample Name`)
  unknown.mean <- data.frame(matrix(NA,nrow = length(samples),ncol = length(targets)))
  row.names(unknown.mean) <- samples
  colnames(unknown.mean) <- targets
  unknown.sd <- unknown.mean # copy empty dataframe
  # calculate mean and sd for each target/dilution pair
  # functional but probably inefficient solution with for loops
  for(x in samples){
    for(y in targets){
      z <- unknown[unknown$`Sample Name`==x & unknown$`Target Name`==y,]$Cт
      unknown.mean[x,y] <- mean(z)
      unknown.sd[x,y] <- sd(z)
    }
  }
  if(length(z)>2 & auto.outlier==T){
    outlier.report <- outlier(unknown,unknown.mean,unknown.sd,threshold = 0.1,is.standard = F)
  }
  # clean up environment
  rm(samples,targets,unknown,x,y,z)
}




