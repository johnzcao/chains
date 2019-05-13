# Auto detect outliers
# Test pair
#x <- highsd[1,]

testOutlier <- function(x,data, is.stand=FALSE){ # x must be a pair of Sample/Target names
  if(is.stand==TRUE){
    cts <- data[data$`Quantity`==as.numeric(x[1]) & data$`Target Name`==x[2],]
  }else{
    cts <- data[data$`Sample Name`==x[1] & data$`Target Name`==x[2],]
  }
  n <- nrow(cts)
  # Added additional condition since if any pairs have less than 3 would create errors (sd() needs at least 2 values)
  if(n<3){
    return(list("well"="undetermined","sample"=x[[1]],"target"=x[[2]],
                "old_mean"=mean(cts$Cт),"old_sd"=sd(cts$Cт),
                "new_mean"="undetermined","new_sd"="undetermined"))
  }else{
    minus1sd <- lapply(1:n, function(y) sd(cts[-y,]$Cт))
    # Identify unique outlier. Does not change mean and sd if a unique outlier cannot be found.
    # Unique outlier is defined as having the only value that can reduce sd when excluded.
    if(sum(unlist(minus1sd) < sd(cts$Cт)) > 1){ # exclusion of more than one value improves sd
      return(list("well"="undetermined","sample"=x[[1]],"target"=x[[2]],
                  "old_mean"=mean(cts$Cт),"old_sd"=sd(cts$Cт),
                  "new_mean"="undetermined","new_sd"="undetermined"))
    }else{ # A unique outlier can be identified
      if("Well" %in% colnames(cts)){ # only run when Well column is avaialble
        names(minus1sd) <- cts$Well 
      }
      outlier <- names(which.min(unlist(minus1sd)))
      new.cts <- cts[cts$Well != outlier,]$Cт
      new.mean <- mean(new.cts)
      new.sd <- sd(new.cts)
      return(list("well"=outlier,"sample"=x[[1]],"target"=x[[2]],
                  "old_mean"=mean(cts$Cт),"old_sd"=sd(cts$Cт),
                  "new_mean"=new.mean,"new_sd"=new.sd))
    }
  }
}

modify <- function(x){ # x is a list created by testOutlier function
  report[report$Sample==x$sample & report$Target==x$target,]$Outlier <- x$well
}

outlier <- function(data,data.mean,data.sd,threshold=0.3,is.standard=FALSE){
  if(is.standard==TRUE){
    data.mean <- data.mean[,-1]
    data.sd <- data.sd[,-1]
  }
  # Detect high SD
  highsd <- which(data.sd > threshold, arr.ind = TRUE)
  if(nrow(highsd)>0){
    highsd[,"col"] <- colnames(data.sd)[highsd[,"col"]]
    highsd[,"row"] <- row.names(highsd) 
    row.names(highsd) <- 1:nrow(highsd) # rename is necessary to avoid duplicates in the results
    colnames(highsd) <- c("sample","target")
    # apply the detection function to identify the well that cause the most SD
    results <- apply(highsd,1,function(x) testOutlier(x,data,is.stand = is.standard))
    # directly change values in mean and sd table
    report <- data.frame(matrix(NA,nrow = nrow(highsd), ncol = 7))
    colnames(report) <- c("Sample","Target","Outlier","old_mean","old_sd","new_mean","new_sd")
    report$Sample <- highsd[,"sample"]
    report$Target <- highsd[,"target"]
    report$Outlier <- unlist(lapply(results, function(x) x[1]))
    report$old_mean <- unlist(lapply(results, function(x) x[4]))
    report$old_sd <- unlist(lapply(results, function(x) x[5]))
    report$new_mean <- unlist(lapply(results, function(x) x[6]))
    report$new_sd <- unlist(lapply(results, function(x) x[7]))
    return(report)
  }
}

#threshold=0.3
#data <- standard
#data.mean <- standard.mean
#data.sd <- standard.sd
#is.standard <- F

#data <- unknown
#data.mean <- unknown.mean
#data.sd <- unknown.sd
