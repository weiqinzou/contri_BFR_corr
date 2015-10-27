library("plyr",lib.loc="~/myOwnRPackage")

#run example:
#Rscript --slave getValidBugs.R rptInfo_E|M_C rpt_thr E|M_BFR_validRpt 

argv<-commandArgs(TRUE)
rpt_fin_C<-argv[1]
rpt_thr<-argv[2]
contri_fout<-argv[3]#file out for validRpt stat

inactiveRemoval<-function(t,rpt_thr){
#dataframe t contains:who,when,BFR,rptCnt,validRptCnt
    tt<-ddply(t,.(who),summarize,rptTotal=sum(rptCnt))
    p<-unique(tt[as.numeric(tt$rptTotal)>=as.numeric(rpt_thr),]$who)
    t<-t[t$who %in% p,]
    return(t)
}
#dataframe t's format:"who","when","BFR","rptCnt","validRptCnt"
preValidrpt_BFR<-function(t,contri_fout){
    rptors<-unique(t$who)
    head<-c("who","when","BFR","rptCnt","validRptCnt","preValidRptCnt\n")
    cat(head,file=contri_fout,sep=",",append=F)

    for(c in rptors){
        contri<-t[t$who==c,]
        contri<-contri[order(contri$who,contri$when),]
        contri<-ddply(contri,.(who),transform,cntPre=cumsum(validRptCnt)-validRptCnt)
        write.table(contri,contri_fout,sep=",",row.names=F,col.names=F,append=T)
    }

}
#validrpt_BFR(BFR_rfc,contri_fout)
get_rptor_BFR_rpt<-function(rpt_d){
#rpt_d is a dataframe which contains:bug_id,reporter,resolution,creation_ts
    rd<-ddply(rpt_d,.(reporter,year=substr(creation_ts,1,4)),summarize,
          BFR=length(resolution[tolower(resolution)=="fixed"])/length(resolution),
          rptCnt=length(resolution),
          validRptCnt=length(resolution[tolower(resolution)=="fixed"])
          )
    names(rd)<-c("who","when","BFR","rptCnt","validRptCnt")
    return(rd)
}
#get each contributor's contribution by year. without divided by products.
#note that the rpt file is all closed bugs containing file,i.e.rptInfo_E|M_C
rpt_contri<-function(rpt_fin,rpt_thr,contri_fout){
    r<-read.csv(rpt_fin,header=T,sep=",")
    r<-r[,c("bug_id","reporter","creation_ts","resolution")]
    r<-get_rptor_BFR_rpt(r)
    r<-inactiveRemoval(r,rpt_thr)
    preValidrpt_BFR(r,contri_fout)
}
rpt_contri(rpt_fin_C,rpt_thr,contri_fout)
