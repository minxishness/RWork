best <- function(state, outcome) {
## Read outcome data
outcv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

## Check that state and outcome are valid
if (!(is.element(state, outcv[ ,7])))
{
stop("invalid state")
}
validoc<-data.frame(illness=c("heart attack", "heart failure", "pneumonia"),code=c(11,17,23))

if (length(which(validoc==outcome, arr.ind=F))==0)
{
stop("invalid outcome")
}

## Return hospital name in that state with lowest 30-day death
## rate
cond<-which(validoc==outcome,arr.ind=T)

condcode<-validoc[cond[1],2]

hospdata <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
hospdata<-hospdata[order(hospdata[ ,2],decreasing=F),]

##hospdata[ ,condcode]
posit<-which(hospdata[ ,condcode]==min(hospdata[ ,condcode],na.rm=T), arr.ind=T)
hospdata[posit,2]
}