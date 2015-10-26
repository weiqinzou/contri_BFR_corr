library("plyr",lib.loc="~/myOwnRPackage")
library("ggplot2",lib.loc="~/myOwnRPackage")
library("labeling",lib.loc="~/myOwnRPackage")

#run example:
#Rscript --slave sameStart.R E_BFR_rfc|M_BFR_rfc 10

argv<-commandArgs(TRUE)
BFR_rfc<-argv[1]
rpt_thr<-argv[2]#using this value to filter inactive reporter of each year.
source("./util.R")


sameStartLevel<-function(BFR_rfc_fin,rpt_thr){
    t<-read.table(BFR_rfc_fin,header=T,sep=",")
#since cal BFR corr with preContri, we remove reporters who only reported
#for just one year
    cc<-ddply(t,.(who),summarize,contriCnt=length(when))
    cc<-cc[cc$contriCnt>=2,]
    t<-t[t$who %in% cc$who,]

    t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]
    tt<-ddply(t,.(who),transform,minY=min(when))
    tt<-tt[tt$when==tt$minY,]
    tt$bzone<-cut(tt$BFR,seq(0,1,0.1),labels=F)
    tt[is.na(tt)]<-1#when BFR=0, the cut func will give a NA to its zone, we reset  its zone to 1
    #p<-ggplot(tt,aes(x=rptCnt))+geom_histogram()+facet_wrap(~bzone,scales="free")
    #print(p)
    
    tt<-tt[,c("who","bzone")]
    t<-merge(t,tt,by="who",all.x=T)
    bz<-sort(unique(t$bzone))
    for(z in bz){
        level<-t[t$bzone == z,]
        print(z)
        latest_cal_corr(level)
        print
    }
    
}
#sameStartLevel(BFR_rfc,rpt_thr)

sameStart_relativeWhen<-function(BFR_rfc_fin,rpt_thr){
    t<-read.table(BFR_rfc_fin,header=T,sep=",")
    t<-relativeWhen(t)
    t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]

    tt<-ddply(t,.(who),transform,minY=min(when))
    tt<-tt[tt$when==tt$minY,]
    tt$bzone<-cut(tt$BFR,seq(0,1,0.1),labels=F)
    tt[is.na(tt)]<-1#when BFR=0, the cut func will give a NA to its zone, we reset  its zone to 1
    
    tt<-tt[,c("who","bzone")]
    t<-merge(t,tt,by="who",all.x=T)
    
    bz<-sort(unique(t$bzone))
    #dataframe t's format:"who","when","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","whenRank","whenDiff","bzone"
    for(z in bz){
        level<-t[t$bzone == z,]
        print(z)
        #remove the bzone col to correclty call the relativeWhen_corr function
        level<-level[,c(-12)]
        print(nrow(level))
        relativeWhen_corr(level)
        print
    }
    
}
sameStart_relativeWhen(BFR_rfc,rpt_thr)


