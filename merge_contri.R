library('plyr',lib.loc="~/myOwnRPackage")

#run example:
#Rscript --slave merge_contri_year.R rptInfo_M_BFR rptInfo_M_preCnt fixInfo_M_preCnt cmtInfo_M_preCnt rfc_M_preCnt&

#Rscript --slave merge_contri_year.R rptInfo_E_BFR rptInfo_E_preCnt fixInfo_E_preCnt cmtInfo_E_preCnt rfc_E_preCnt&

argv<-commandArgs(TRUE)
rpt_preCnt<-argv[1]
fix_preCnt<-argv[2]
cmt_preCnt<-argv[3]
rpt_BFR<-argv[4]
rfc_preCnt<-argv[5]

#normalize the numeric value
range01<-function(x){
  rng<-range(x,na.rm=TRUE)
  (x-rng[1])/diff(rng)
}


#input data format:rpt|fix|cmt_preCnt contains: who, when, cnt,cntPre
#rptBFR contains:who,when,BFR
merge_preCnt<-function(rPreCnt_fin,fPreCnt_fin,cPreCnt_fin,
                       rptBFR_fin,rfc_fout){
    r<-read.csv(rPreCnt_fin,header=T,sep=",")
    names(r)<-c("who","when","rptCnt","rptCntPre")
    f<-read.csv(fPreCnt_fin,header=T,sep=",")
    names(f)<-c("who","when","fixCnt","fixCntPre")
    c<-read.csv(cPreCnt_fin,header=T,sep=",")
    names(c)<-c("who","when","cmtCnt","cmtCntPre")
    b<-read.csv(rptBFR_fin,header=T,sep=",")
    #merge rpt|fix|cmt preCnt with BFR
    br<-merge(b,r,by=c("who","when"),all.x=T)
    brf<-merge(br,f,by=c("who","when"),all.x=T)
    brfc<-merge(brf,c,by=c("who","when"),all.x=T)
    write.table(brfc,rfc_fout,sep=",",col.names=T,row.names=F)
}

test_cor<-function(rfc_preCnt_fin){
    rfc_preCnt<-read.csv(rfc_preCnt_fin,header=T,sep=",")
    year<-unique(rfc_preCnt$when)
    for(y in year){
        print(y)
        rfc<-rfc_preCnt[rfc_preCnt$when==year,]
        #plot(rfc$BFR~range01(rfc$rptCntPre))
        #plot(rfc$BFR~range01(rfc$fixCntPre))
        #plot(rfc$BFR~range01(rfc$cmtCntPre))

        rfc[is.na(rfc)]<-0
        plot(rfc$BFR~range01(rfc$rptCntPre))
        plot(rfc$BFR~range01(rfc$fixCntPre))
        plot(rfc$BFR~range01(rfc$cmtCntPre))


    }

}
#merge_preCnt(rpt_preCnt,fix_preCnt,cmt_preCnt,rpt_BFR,rfc_preCnt)


test_cor(rfc_preCnt)
