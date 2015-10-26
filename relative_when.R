library('plyr',lib.loc="~/myOwnRPackage")
source('./util.R')

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
print("whenRank's corr:")
wr<-t[,c(-2,-11)]#remove the when and whenDiff cols
names(wr)<-c("who","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","when")
cal_corr_year(wr)
print("whenDiff's corr:")
wd<-t[,c(-2,-10)]
names(wd)<-c("who","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","when")
cal_corr_year(wd)
