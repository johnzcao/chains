# Main script for data processing

# Test data set(s)
data.path <- c("Test_data_1.xls","Test_data_2.xls")
data.path <- c("Test_data_3.xls")
data.path <- c("Test_data_4.xls")
data.path <- c("TF-1 data.xlsx")

library(dplyr)
# Define analysis mode and controls. May be interactive in future versions
analysis.method <- "DD" # Plan to have: DD = ΔΔCт, D = ΔCт, chip = percent input, Q = quantity
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
  # functional but probably inefficient solution with for loops
  for(x in samples){
    for(y in targets){
      z <- unknown[unknown$`Sample Name`==x & unknown$`Target Name`==y,]$Cт
      unknown.mean[x,y] <- mean(z)
      unknown.sd[x,y] <- sd(z)
    }
  }
  if(length(z)>2 & auto.outlier==T){
    outlier.report <- outlier(unknown,
                              unknown.mean,
                              unknown.sd,
                              threshold = SD.threshold,
                              is.standard = F)
    if(!is.null(outlier.report)){
      for(k in 1:nrow(outlier.report)){
        samp <- outlier.report[k,1]
        targ <- outlier.report[k,2]
        newm <- outlier.report[k,4]
        newsd <- outlier.report[k,5]
        unknown.mean[samp,targ] <- newm
        unknown.sd[samp,targ] <- newsd
      }
    }
  }
  
  # clean up environment
  rm(samples,targets,unknown,x,y,z)
}

# delta-delta Ct method

hkCt <- unknown.mean[,hkg]

unknown.dCt <- apply(unknown.mean,2,function(x) x-hkCt) %>% as.data.frame()

pct.hkg <- 2^-unknown.dCt %>% as.data.frame()

ctrCt <- unknown.dCt[ctr,] %>% as.numeric()
unknown.FC <- apply(unknown.dCt,1,function(x) E^-(x-ctrCt)) %>% t() %>% as.data.frame()


