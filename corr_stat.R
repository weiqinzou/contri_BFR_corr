options(digits=3)
library('plyr',lib.loc="~/myOwnRPackage")
argv<-commandArgs(TRUE)
contri_stat<-argv[1]

#Rscript --slave corr_stat.R contri_rfc_E|contri_rfc_M

range01<-function(x){
  rng<-range(x,na.rm=TRUE)
  (x-rng[1])/diff(rng)
}

#here corr is a vector or list
get_spearLevel<-function(corr){
    b<-c(0,0.1,0.3,0.5,0.7,0.9,1.0)
    l<-c("None","Small","Moderate","High","Very High","Perfect")
    lidx<-cut(abs(corr),b,labels=F)
    return(l[lidx])
}
#messure rpt|fix|cmt seperately correlation with each reporter's BFR by year.
corr_cal_independent<-function(stat_d){
#stat_d is a dataframe which contains following items:
#"who","when","BFR","rptCnt","fixCnt","cmtCnt","rptCntPre","fixCntPre","cmtCntPre"
    res<-ddply(stat_d,.(when),summarize,    
    rpt_corr=cor.test(x=range01(rptCntPre),y=BFR,method="spearman")$estimate,
    rpt_level=get_spearLevel(cor.test(x=range01(rptCntPre),y=BFR,method="spearman")$estimate),
    rpt_p_value=cor.test(x=range01(rptCntPre),y=BFR,method="spearman")$p.value,
    #rptorCnt=length(who),

    fix_corr=cor.test(x=range01(fixCntPre),y=BFR,method="spearman")$estimate,
    fix_level=get_spearLevel(cor.test(x=range01(fixCntPre),y=BFR,method="spearman")$estimate),
    fix_p_value=cor.test(x=range01(fixCntPre),y=BFR,method="spearman")$p.value,
    #fixerCnt=length(who),

    cmt_corr=cor.test(x=range01(cmtCntPre),y=BFR,method="spearman")$estimate,
    cmt_level=get_spearLevel(cor.test(x=range01(cmtCntPre),y=BFR,method="spearman")$estimate),
    cmt_p_value=cor.test(x=range01(cmtCntPre),y=BFR,method="spearman")$p.value)
    #cmtorCnt=length(who))


    return(res) 

}

#messure rpt|fix|cmt as a whole's correlation with each reporter's BFR by year.
corr_cal_mergeRFC<-function(stat_d){
#stat_d is a dataframe which contains following items:
#"who","when","BFR","rptCnt","fixCnt","cmtCnt","rptCntPre","fixCntPre","cmtCntPre"
####todo tomorrow
}

