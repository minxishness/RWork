rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
#   if (num=="best"){
#     rank<-1}
#   else if (num=="worst")
#   {rank<-sum(!is.na(hospd[,condcode]))
#    rank}
#   else 
#   {rank<-as.numeric(as.character(num))
#   }
outcv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


validoc<-data.frame(illness=c("heart attack", "heart failure", "pneumonia"),code=c(11,17,23))

if (length(which(validoc==outcome, arr.ind=F))==0)
{
stop("invalid outcome")
}

cond<-which(validoc==outcome,arr.ind=T)

condcode<-validoc[cond[1],2]
reso<-data.frame(State=character(),Hospital=character())
reso<-NULL
hospdata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=F, na.strings="Not Available")
hospdata<-hospdata[order(hospdata[,2],decreasing=F),]
temr<-split(hospdata,hospdata$State,drop=T)

##RankOne(temr1,condcode,num)
for (i in 1:length(temr))
{
  temr1<-temr[i]
  temr1<-as.data.frame(temr1)
  temr2<-RankOne(temr1,condcode,num)
  reso<-rbind(reso,c(temr2,temr1[1,7]))
  
}
as.data.frame(reso)
colnames(reso)<-c("hospital","state")
as.data.frame(reso)
}