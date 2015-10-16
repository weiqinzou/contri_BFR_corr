library("plyr",lib.loc="~/myOwnRPackage")
#run example:
#Rscript --slave plot_cor.R E_BFR_rfc|M_BFR_rfc

argv<-commandArgs(TRUE)
BFR_rfc<-argv[1]
rpt_thr<-argv[2]#using this value to filter inactive reporter of each year.
#here corr is a vector or list
get_spearLevel<-function(corr){
    b<-c(0,0.1,0.3,0.5,0.7,0.9,1.0)
    l<-c("None","Small","Moderate","High","Very High","Perfect")
    lidx<-cut(abs(corr),b,labels=F)
    return(l[lidx])
}
#cal rpt|fix|cmt 's cnt's corr with BFR seperately
#since spearman corr use spearman rank to cal the correlation.
#there is no need to normalize the cnt data.
cal_CntBFR_corr<-function(contri_d){
    names(contri_d)<-c("who","when","BFR","cnt")
    uC=length(unique(contri_d$cnt))
    if(uC<2) return
    
    res<-data.frame(
    cnt_corr=cor.test(x=contri_d$cnt,y=contri_d$BFR,method="spearman")$estimate,
    cnt_level=get_spearLevel(cor.test(x=contri_d$cnt,y=contri_d$BFR,method="spearman")$estimate),
    cnt_p_value=cor.test(x=contri_d$cnt,y=contri_d$BFR,method="spearman")$p.value,
    uniqueCnt=length(unique(contri_d$cnt)))
    
    return(res)
}

cal_corr<-function(BFR_rfc_fin,rpt_thr){
    t<-read.table(BFR_rfc_fin,header=T,sep=",")
    t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]

    t<-ddply(t,.(who),transform,maxY=max(when))
    t<-t[t$when==t$maxY,]
    
    for(type in c("rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt")){
        tY<-t[,c("who","when","BFR",type)]
    
        cor_res<-cal_CntBFR_corr(tY)
        cor_res$contri_type<-type
        print(cor_res)
    }
}
cal_corr(BFR_rfc,rpt_thr)
