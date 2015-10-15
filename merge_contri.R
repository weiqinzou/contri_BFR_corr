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

merge_preCnt(rpt_preCnt,fix_preCnt,cmt_preCnt,rpt_BFR,rfc_preCnt)
