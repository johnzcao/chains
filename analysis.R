# Analysis module
# required input: 
#   E (efficiency), 
#   hkg (housekeeping gene), 
#   ctr (control sample), 
#   analysis.method, 
#   unknown.mean
# analysis methods:
#   ddct (delta-delta Ct)
#   dCt (delta Ct)
#   chip (percent input)
#   quant (quantity)
# standardized output:
#   norm.ct (normalized Ct based on method)
#   rel.quant (FC/percent/quantity based on method)


if(grepl("dct",analysis.method,fixed = T)){
  if(!exists("hkg")){
    print("Housekeeping gene (hkg) undefined!")
  }else{
    hkCt <- unknown.mean[,hkg]
    unknown.dCt <- apply(unknown.mean,2,function(x) x-hkCt) %>% as.data.frame()
  }
  if(analysis.method=="ddct"){
    if(!exists("ctr")){
      print("Control sample (ctr) undefined!")
    }else{
      ctrCt <- unknown.dCt[ctr,] %>% as.numeric()
      norm.ct <- apply(unknown.dCt,1,function(x) x-ctrCt) %>% t() %>% as.data.frame()
      rel.quant <- E^-norm.ct %>% as.data.frame()
      rm(unknown.dCt,hkCt,ctrCt)
    }
  }else{
    norm.ct <- unknown.dCt
    rel.quant <- E^-norm.ct %>% as.data.frame()
    rm(unknown.dCt,hkCt)
  }
}else if(analysis.method=="chip"){
  # to be added
}else if(analysis.method=="quant"){
  # tobe added
}else{
  print("Analysis method not found.")
}



