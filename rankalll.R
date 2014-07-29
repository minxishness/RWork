rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  if (num=="best"){
    rank<-1}
  else if (num=="worst")
  {rank<-sum(!is.na(hospd[,condcode]))
   rank}
  else 
  {rank<-as.numeric(as.character(num))
  }
outcv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


validoc<-data.frame(illness=c("heart attack", "heart failure", "pneumonia"),code=c(11,17,23))

if (length(which(validoc==outcome, arr.ind=F))==0)
{
stop("invalid outcome")
}

cond<-which(validoc==outcome,arr.ind=T)

condcode<-validoc[cond[1],2]

hospdata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=F, na.strings="Not Available")
hospdata<-hospdata[order(hospdata[,condcode],decreasing=F),]
temr<-split(hospdata,hospdata$State,drop=T)
str(temr)

for (i in 1:length(temr))
{
  temr1<-temr[i]
  print(temr1[rank,2])
}
}