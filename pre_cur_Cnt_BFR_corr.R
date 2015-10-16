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
#cal rpt|fix|cmt 's cnt's corr with BFR seperately by year.
#since spearman corr use spearman rank to cal the correlation.
#there is no need to normalize the cnt data.
cal_CntBFR_corr_year<-function(contri_d){
    names(contri_d)<-c("who","when","BFR","cnt")
    d<-ddply(contri_d,.(when),summarize,uC=length(unique(cnt)))
    y<-d[d$uC>1,]$when#for spearman corr, we need >1 unique cnt values
    contri_d<-contri_d[contri_d$when %in% y,]
    
    res<-ddply(contri_d,.(when),summarize,    
    cnt_corr=cor.test(x=cnt,y=BFR,method="spearman")$estimate,
    cnt_level=get_spearLevel(cor.test(x=cnt,y=BFR,method="spearman")$estimate),
    cnt_p_value=cor.test(x=cnt,y=BFR,method="spearman")$p.value,
    uniqueCnt=length(unique(cnt)))
    
    return(res)
}

cal_corr_year<-function(BFR_rfc,rpt_thr){
    t<-read.table(BFR_rfc,header=T,sep=",")
    t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]
    for(type in c("rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt")){
        tY<-t[,c("who","when","BFR",type)]
    
        cor_res<-cal_CntBFR_corr_year(tY)
        cor_res$contri_type<-type
        print(cor_res)
    }
}
cal_corr_year(BFR_rfc,rpt_thr)
