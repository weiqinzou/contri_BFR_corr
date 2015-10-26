library('plyr',lib.loc="~/myOwnRPackage")

argv<-commandArgs(TRUE)
BFR_rfc<-argv[1]
rpt_thr<-argv[2]#using this value to filter inactive reporter of each year.

relativeWhen<-function(bfr_d){
    tt<-ddply(bfr_d,.(who),transform,whenRank=rank(when),whenDiff=when-min(when))
    return(tt)
}

t<-read.csv(BFR_rfc,header=T,sep=",")
t<-relativeWhen(t)

t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]
#header for t:
#"who","when","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","whenRank","whenDiff"
wr<-t[,c(-2,-11)]#remove the when and whenDiff cols
names(wr)<-c("who","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","when")

wd<-t[,c(-2,-10)]
names(wd)<-c("who","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","when")


