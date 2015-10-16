library('plyr',lib.loc="~/myOwnRPackage")

#run example:
#Rscript --slave merge_contri_year.R rptInfo_M_BFR M_rpt M_fix M_cmt M
#Rscript --slave merge_contri_year.R rptInfo_E_BFR E_rpt E_fix E_cmt E

argv<-commandArgs(TRUE)
rpt_BFR<-argv[1]
rpt_preCnt<-argv[2]
fix_preCnt<-argv[3]
cmt_preCnt<-argv[4]
proj_name<-argv[5]

#input data format:rpt|fix|cmt_preCnt contains: who,when,curCnt,preCnt
#rptBFR contains:who,when,BFR
merge_contri<-function(rptBFR_fin,rPreCnt_fin,fPreCnt_fin,cPreCnt_fin,
                       proj_name){
    b<-read.csv(rptBFR_fin,header=T,sep=",")
    names(b)<-c("who","when","BFR")

    r<-read.csv(rPreCnt_fin,header=T,sep=",")
    names(r)<-c("who","when","rptCnt","preRptCnt")
    f<-read.csv(fPreCnt_fin,header=T,sep=",")
    names(f)<-c("who","when","fixCnt","preFixCnt")
    c<-read.csv(cPreCnt_fin,header=T,sep=",")
    names(c)<-c("who","when","cmtCnt","preCmtCnt")
    
    #merge rpt|fix|cmt preCnt with BFR
    br<-merge(b,r,by=c("who","when"),all.x=T)
    brf<-merge(br,f,by=c("who","when"),all.x=T)
    brfc<-merge(brf,c,by=c("who","when"),all.x=T)
    rfc_fout<-paste(proj_name,'BFR','rfc',sep='_')
    write.table(brfc,rfc_fout,sep=",",col.names=T,row.names=F)
}

merge_contri(rpt_BFR,rpt_preCnt,fix_preCnt,cmt_preCnt,proj_name)
