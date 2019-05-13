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
    CTy <-standards[standards$`Target Name`==i & standards$Quantity==j,]$Cт
    meany <- mean(CTy)
    matrixydata[j,i] <-meany
    sty <- sd(CTy)
    stdevydata[j,i] <- sty
    }
}

matrixydata$Quantity <- log2(as.numeric(matrixydata$Quantity))
#quantitiesln <- log2(as.numeric(quantities))
#matrixydata$Quantity <- quantitiesln

for (i in matrixydata){
  print(cor(as.numeric(matrixydata$Quantity),as.numeric(i)))
}
cor(as.numeric(matrixydata$Quantity),as.numeric(matrixydata$`18S`))
for (i in matrixydata){
  slope<- lm(i~matrixydata$Quantity)$coefficients[[2]]
  print(2^(-1/slope))
}


