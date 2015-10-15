library('plyr',lib.loc="~/myOwnRPackage")

#run example:
#Rscript --slave get_contri_year.R rptInfo_M fixInfo_M cmtInfo_M 10 
#Rscript --slave get_contri_year.R rptInfo_E fixInfo_E cmtInfo_E 10

argv<-commandArgs(TRUE)
ori_rpt<-argv[1]
ori_fix<-argv[2]
ori_cmt<-argv[3]
rpt_thr<-argv[4]


#get all closed bug reports
get_rpt_closed<-function(ori_rpt){
   ori<-read.csv(ori_rpt,header=TRUE,sep=',')
   stat<-tolower(ori$bug_status)
   ori<-ori[stat=="closed" | stat=="resolved" | stat=="verified",]
   return(ori)
}

#return the reporter name list who reported more than "rpt_thr" bugs
get_active_rptor<-function(ori_rpt,rpt_thr){
   ori<-get_rpt_closed(ori_rpt)
   r<-ddply(ori,.(reporter),summarize,cnt=length(bug_id))
   r<-r[as.numeric(r$cnt)>=as.numeric(rpt_thr),]$reporter
   return(r)
}

#cal each reporter's BFR by year
get_rptor_BFR<-function(rpt_fin,rpt_thr){
   
    rpt_d<-get_rpt_closed(rpt_fin)
    active_rptor<-get_active_rptor(rpt_fin,rpt_thr)
    rpt_d<-rpt_d[,c("bug_id","reporter","creation_ts","resolution")]
    rpt_d<-rpt_d[rpt_d$reporter %in% active_rptor,]
    rd<-ddply(rpt_d,.(reporter,year=substr(creation_ts,1,4)),summarize,
          BFR=length(resolution[tolower(resolution)=="fixed"])/length(resolution))
    names(rd)<-c("who","when","BFR")
    write.table(rd,paste(rpt_fin,"BFR",sep="_"),sep=",",col.names=T,row.names=F) 
}

#get each contributors' each year's contributions and previous contributions
#rfc_d is a dataframe which contains:bug_id,who,when
#active_rptor is a reporter list who reported less than N bug reports totally
#contri_type indicates which kinds of contributions you are calculating for 
#the active rptors
get_contri<-function(rfc_d,active_rptor,contri_type){

    active_rptor<-unique(active_rptor)
    t<-rfc_d
    t<-t[t$who %in% active_rptor,]
    t$when<-substr(t$when,1,4)
    years<-unique(t$when)
    
    out<-paste(rfc_fin,contri_type,sep='_')
    head<-c("who","when","curCnt","preCnt\n")
    cat(head,file=out,sep=",",append=F)
    for(y in years){
        contri<-dataframe(who=active_rptor)
        contri$when<-y

        cur<-t[as.numeric(t$when)==as.numeric(y),]
        pre<-t[as.numeric(t$when)<as.numeric(y),]
        curc<-ddply(cur,.(who),summarize,curCnt=length(bug_id))
        prec<-ddply(pre,.(who),summarzie,preCnt=length(bug_id))

        contri<-merge(contri,curc,by=c("who"),all.x=T)
        contri<-merge(contri,prec,by=c("who"),all.x=T)
        contri[is.na(contri)]<-0

        write.table(contri,contri_fout,sep=",",row.names=F,col.names=F,append=T)
    }
}

total_contri<-function(rpt_fin,fix_fin,cmt_fin,rpt_thr){
    active_rptor<-get_active_rptor(rpt_fin,rpt_thr)

    r<-get_rpt_closed(rpt_fin)
    r<-r[,c("bug_id","reporter","creation_ts")]
    names(r)<-c("bug_id","who","when")
    get_contri(r,active_rptor,"rpt")

    f<-read.csv(fix_fin,header=T,sep=",")
    get_contri(f,active_rptor,"fix")

    c<-read.csv(cmt_fin,header=T,sep=",")
    get_contri(c,active_rptor,"cmt")

}

get_rptor_BFR(ori_rpt,rpt_thr)
total_contri(ori_rpt,ori_fix,ori_cmt,rpt_thr)

