library("plyr",lib.loc="~/myOwnRPackage")
#run example:
#Rscript --slave multi_logi,R E|M_BFR_rpt E|M_validRpt 10

argv<-commandArgs(TRUE)
BFR_rfc<-argv[1]
BFR_validRpt<-argv[2]
rpt_thr<-argv[3]

merge_rfc_validRpt<-function(BFR_rfc,BFR_validRpt){
#BFR_rfc contains:
#"who","when","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt"
#BFR_validRpt contains:who,when,BFR,rptCnt,validRptCnt,preValidRptCnt

    rfc<-read.csv(BFR_rfc,header=T,sep=",")

    validRpt<-read.csv(BFR_validRpt,header=T,sep=",")
    validRpt<-validRpt[,c("who","when","validRptCnt","preValidRptCnt")]
    valid_rfc<-merge(rfc,validRpt,by=c("who","when"),all.x=T)
    valid_rfc[is.na(valid_rfc)]<-0
#    write.table(valid_rfc,'valid_rfc.merge',sep=",",col.names=T,row.names=F)
    return(valid_rfc)
}

range01<-function(x){
  rng<-range(x,na.rm=TRUE)
  (x-rng[1])/diff(rng)
}

range01B4corr<-function(d){
    for(colName in colnames(d)){
       d[,colName]<-range01(d[,colName]) 
    }
    return(d)
}

rankB4corr<-function(d){
    for(colName in colnames(d)){
       d[,colName]<-rank(d[,colName]) 
    }
    return(d)
}

corr_stat<-function(d){
    #here d is a dataframe which contains:
    #"BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","validRptCnt","preValidRptCnt"
    #########!!!!!!!!two methods to preprocess the d, one is rank,the other is range01,
    #here, since we cal lm,I think range01 is more rational
    #d<-rankB4corr(d)#########!!!!!!!!two methods to preprocess the d, one is rank,the other is
    d<-range01B4corr(d)
    print("multi lm corr with cur contri with BFR:")
    cur<-d[,c("BFR","rptCnt","validRptCnt","fixCnt","cmtCnt")]
    #cor btw two variables:
    print(summary(cor(cur)))
    cur_lm<-lm(BFR~rptCnt+validRptCnt+fixCnt+cmtCnt,cur)
    print(summary(cur_lm))
    cur_lm_step<-step(cur_lm)
    print(summary(cur_lm_step))
    
    print("multi lm corr with pre contri with BFR:")
    pre<-d[,c("BFR","preRptCnt","preValidRptCnt","preFixCnt","preCmtCnt")]
    #cor btw two variables:
    print(summary(cor(pre)))
    pre_lm<-lm(BFR~preRptCnt+preValidRptCnt+preFixCnt+preCmtCnt,pre)
    #print(summary(pre_lm))
    pre_lm_step<-step(pre_lm)
    print(summary(pre_lm_step))
    
    #!!!!!!!!!!!!!!!!!!!AIC is -infinity for this model, so 'step' cannot proceed
    #!!!!!!!!!!!!!!!!!!!met with this issue, we wont cal pre_cur corr at the moment 
    #print("multi lm corr with pre_cur contri with BFR:")
    ##cor btw two variables:
    #print(summary(cor(d)))
    #d_lm<-lm(BFR~rptCnt+validRptCnt+fixCnt+cmtCnt+preRptCnt+preValidRptCnt+preFixCnt+preCmtCnt,d)
    #print(summary(d_lm))
    #d_lm_step<-step(d_lm)
    #print(summary(d_lm_step))
}
latest_multi_corr<-function(t){
    t<-ddply(t,.(who),transform,maxY=max(when))
    t<-t[t$when==t$maxY,]
    t<-t[,c("BFR","rptCnt","preRptCnt","fixCnt","preFixCnt",
            "cmtCnt","preCmtCnt","validRptCnt","preValidRptCnt")]
    corr_stat(t)
}

when_corr<-function(d){
#dataframe d must contains:
#"when","BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt","validRptCnt","preValidRptCnt"
    when<-unique(d$when)
    for(w in when){
        wd<-d[d$when==w,]
        wd<-wd[,c("BFR","rptCnt","preRptCnt","fixCnt","preFixCnt",
           "cmtCnt","preCmtCnt","validRptCnt","preValidRptCnt")]
        flag=FALSE
        for(cname in colnames(wd)){
            if(length(unique(wd[,cname]))<2)flag=TRUE
        }
        if(!flag){
            print(w)
            corr_stat(wd)
        }
    }
}
absolute_multi_corr<-function(d){
    when_corr(d) 
}

relativeWhen<-function(d){
    t<-ddply(d,.(who),transform,whenRank=rank(when),whenDiff=when-min(when))
    return(t)
}

relative_multi_corr<-function(d){
    t<-relativeWhen(d)
    print("whenRank's corr:")
    wr<-t[,c("BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt",
             "validRptCnt","preValidRptCnt","whenRank")]
    names(wr)<-c("BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt",
                 "validRptCnt","preValidRptCnt","when")
    when_corr(wr)

    print("whenDiff's corr:")
    wd<-t[,c("BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt",
             "validRptCnt","preValidRptCnt","whenDiff")]
    names(wd)<-c("BFR","rptCnt","preRptCnt","fixCnt","preFixCnt","cmtCnt","preCmtCnt",
                 "validRptCnt","preValidRptCnt","when")
    when_corr(wd)
}

corr_multi<-function(d){
     latest_d<-d
     latest_multi_corr(latest_d)   

     absolute_d<-d
     absolute_multi_corr(absolute_d)   

#######!!!!!!!!!!!!just can't understand why this cant work!!!!!!!!!!!!!!!!
#     relative_d<-d
#     relative_multi_corr(relative_d)
}

run<-function(BFR_rfc,BFR_validRpt,rpt_thr){
    d<-merge_rfc_validRpt(BFR_rfc,BFR_validRpt)
    d<-d[as.numeric(d$rptCnt)>=as.numeric(rpt_thr),]
    corr_multi(d)
}

run(BFR_rfc,BFR_validRpt,rpt_thr)

