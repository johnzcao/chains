# Main script for data processing

# Test data set(s)
data.path <- c("Test_data_1.xls","Test_data_2.xls")
data.path <- c("Test_data_3.xls")
data.path <- c("Test_data_4.xls")
data.path <- c("TF-1 data.xlsx")

library(dplyr)
# Define analysis mode and controls. May be interactive in future versions
analysis.method <- "dct" # Plan to have: ddct = ΔΔCт, dct = ΔCт, chip = percent input, Q = quantity
E <- 2 # Default amplification efficiency unless corrected by standards
hkg <- "18S" # Housekeeping gene
ctr <- "TF-1 D1" # control sample to compare to
auto.outlier <- TRUE # auto detect and exclude outliers if at least triplicates
SD.threshold <- 0.3 # default sd threshold for outlier detection

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

# Calculate unknown meansand sd 
if(!"UNKNOWN" %in% names(sep.data)) {
  stop("No Unknown found in data!")
}else{
  unknown <- sep.data[["UNKNOWN"]]
  targets <- levels(unknown$`Target Name`)
  samples <- levels(unknown$`Sample Name`)
  unknown.mean <- matrix(NA,nrow = length(samples),ncol = length(targets)) %>% data.frame()
  row.names(unknown.mean) <- samples
  colnames(unknown.mean) <- targets
  unknown.sd <- unknown.mean # copy empty dataframe
  # calculate mean and sd for each target/dilution pair
  # functional but inefficient solution with for loops
  nrep <- 1
  for(x in samples){
    for(y in targets){
      z <- unknown[unknown$`Sample Name`==x & unknown$`Target Name`==y,]$Cт
      unknown.mean[x,y] <- mean(z)
      unknown.sd[x,y] <- sd(z)
      # this test is to get the maximum number of replicate, in case replicate numbers are uneven
      if(length(z)>nrep){ 
        nrep <- length(z)
      }
    }
  }
  if(nrep>2 & auto.outlier==T){
    outlier.report <- outlier(unknown,
                              unknown.mean,
                              unknown.sd,
                              threshold = SD.threshold,
                              is.standard = F)
    if(!is.null(outlier.report)){
      for(k in 1:nrow(outlier.report)){
        if(outlier.report[k,3]!="undetermined"){
          samp <- outlier.report[k,1]
          targ <- outlier.report[k,2]
          newm <- outlier.report[k,6]
          newsd <- outlier.report[k,7]
          unknown.mean[samp,targ] <- newm
          unknown.sd[samp,targ] <- newsd
          rm(samp,targ,newm,newsd)
        }
      }
    }
  }
  
  # clean up environment
  rm(samples,targets,unknown,x,y,z,nrep)
}

source("analysis.R")



