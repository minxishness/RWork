RankOne <- function(hospd, condcode, num1)
{
if (num1=="best"){
  rank<-1}
else if (num1=="worst")
 {rank<-sum(!is.na(hospd[,condcode]))
rank}
else 
  {rank<-as.numeric(as.character(num1))
        }
hospd<-hospd[order(hospd[,condcode],decreasing=F),]
hospd[rank,2]

}