library('plyr',lib.loc="~/myOwnRPackage")
source('./util.R')

argv<-commandArgs(TRUE)
BFR_rfc<-argv[1]
rpt_thr<-argv[2]#using this value to filter inactive reporter of each year.

t<-read.csv(BFR_rfc,header=T,sep=",")
t<-relativeWhen(t)

t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]
#header for t:
#"who","when","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","whenRank","whenDiff"
relativeWhen_corr(t)
