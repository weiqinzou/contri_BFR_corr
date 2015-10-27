library("plyr",lib.loc="~/myOwnRPackage")
#run example:
#Rscript --slave corr_validRpt.R E|M_validRpt 10

argv<-commandArgs(TRUE)
BFR_validRpt<-argv[1]
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
###########!!!!!!!cal latest active when corr!!!!!!!!!!##########
#BFR_validRpt_fin contains:who,when,BFR,rptCnt,validRptCnt,preValidRptCnt
latest_cal_corr<-function(BFR_validRpt_fin,rpt_thr){
    t<-read.table(BFR_validRpt_fin,header=T,sep=",")

#we only consider reporter's BFR stable if he reported more than rpt_thr closed bugs a year.
    t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]

    t<-ddply(t,.(who),transform,maxY=max(when))
    t<-t[t$when==t$maxY,]
    
    for(type in c("rptCnt","validRptCnt","preValidRptCnt")){
        tY<-t[,c("who","when","BFR",type)]
    
        cor_res<-cal_CntBFR_corr(tY)
        cor_res$contri_type<-type
        print(cor_res)
    }
}

########!!!!!!!!relative when!!!!!!###############
relativeWhen<-function(bfr_d){
    tt<-ddply(bfr_d,.(who),transform,whenRank=rank(when),whenDiff=when-min(when))
    return(tt)
}

#!!!!!!!!!!!!cal relative time corr #############
############!!!!!!!!!!!!!!cal corr by year, here year is a relative concept!!!!!!!!!!!!!!!!
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

cal_corr_year<-function(BFR_rfc_d){
    for(type in c("rptCnt","validRptCnt","preValidRptCnt")){
        tY<-BFR_rfc_d[,c("who","when","BFR",type)]
        cor_res<-cal_CntBFR_corr_year(tY)
        cor_res$contri_type<-type
        print(cor_res)
    }
}

relativeWhen_corr<-function(t){
#here t is a dataframe which contains following items:
#who,when,BFR,rptCnt,validRptCnt,preValidRptCnt,whenRank,whenDiff

    print("whenRank's corr:")
    wr<-t[,c(-2,-8)]#remove the when and whenDiff cols
    names(wr)<-c("who","BFR","rptCnt","validRptCnt","preValidRptCnt","when")
    cal_corr_year(wr)
    print("whenDiff's corr:")
    wd<-t[,c(-2,-7)]
    names(wd)<-c("who","BFR","rptCnt","validRptCnt","preValidRptCnt","when")
    cal_corr_year(wd)
}
###########!!!!!!!cal relative when corr!!!!!!!!!!##########
run_relativeWhen<-function(validRpt_fin,rpt_thr){
    t<-read.csv(validRpt_fin,header=T,sep=",")
    
    t<-relativeWhen(t)
    t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]
    #header for t:
    #who,when,BFR,rptCnt,validRptCnt,preValidRptCnt,whenRank,whenDiff
    relativeWhen_corr(t)
}
###########!!!!!!!cal absolute when corr!!!!!!!!!!##########
run_absoluteWhen<-function(validRpt_fin,rpt_thr){
    t<-read.csv(validRpt_fin,header=T,sep=",")
    
    t<-t[as.numeric(t$rptCnt)>=as.numeric(rpt_thr),]
    #header for t:
    #who,when,BFR,rptCnt,validRptCnt,preValidRptCnt,whenRank,whenDiff
    cal_corr_year(t)   
}

###final run-control function####
run_corr<-function(BFR_validRpt,rpt_thr){
    print("corr btw latest active year's BFR and validRpt contri")
    latest_cal_corr(BFR_validRpt,rpt_thr)
    print("corr btw relative year's BFR and validRpt contri")
    run_relativeWhen(BFR_validRpt,rpt_thr)
    print("corr btw absolute year's BFR and validRpt contri")
    run_absoluteWhen(BFR_validRpt,rpt_thr)
}
run_corr(BFR_validRpt,rpt_thr)
