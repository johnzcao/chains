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

# define variables and make table
standards <- sep.data$STANDARD


library(dplyr)
targets <- levels(standards$`Target Name`)
variablenumber <- length(targets)+1
quantities <- as.factor(standards$Quantity) %>% levels()
observnumber <- length(quantities)
matrixydata <- as.data.frame(matrix(NA,observnumber,variablenumber))
row.names(matrixydata) <- quantities
colnames(matrixydata) <- c("Quantity", targets)
matrixydata$Quantity<- quantities
stdevydata <- matrixydata

#find the mean of the standards in the raw data

# first method of finding the mean of the standards and putting it into the table--for loop

#for (i in quantitylist){
#matrixydata[as.character(i),"Ct mean"]<-as.numeric(standards[standards$`Target Name`=="TET1" & standards$Quantity== i,]$Cт) %>% mean()

#apply lapply method
# step one is set up the function that will find the mean of the ct values  

for (i in targets){
  for (j in quantities){
    CT <-standards[standards$`Target Name`==i & standards$Quantity==j,]$Cт
    mean <- mean(CTy)
    matrixydata[j,i] <-meany
    st <- sd(CTy)
    stdevydata[j,i] <- sty
    rm(CT,mean, st)
  }
}

matrixydata$Quantity <- log2(as.numeric(matrixydata$Quantity))
#quantitiesln <- log2(as.numeric(quantities))
#matrixydata$Quantity <- quantitiesln

report<- list()
for (v in 1:ncol(matrixydata)){
  if(v==1){
    next
  }
  slope<- lm(matrixydata[[v]]~matrixydata$Quantity)$coefficients[[2]]
  efficiency <- 2^(-1/slope)
  correlation <- cor(as.numeric(matrixydata$Quantity),matrixydata[[v]])
  EandC <- c(efficiency,correlation) #make an array of the efficiency and the correlation
  report[colnames(matrixydata[v])]<-list(EandC) #this takes existing (but empty) list 'report' gives it a name in the [] then adds the eandc to the list
  print(report)
  rm(slope, correlation, efficiency)
}
#Keep: report matrixydata, stdev table, 
rm(targets, variablenumber, quantities, observnumber)
