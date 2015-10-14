library('plyr',lib.loc="~/myOwnRPackage")

#run example:
#Rscript --slave get_contri_year.R rptInfo_M fixInfo_M cmtInfo_M 10 contri_rfc_M&
#Rscript --slave get_contri_year.R rptInfo_E fixInfo_E cmtInfo_E 10 contri_rfc_E&

argv<-commandArgs(TRUE)
ori_rpt<-argv[1]
ori_fix<-argv[2]
ori_cmt<-argv[3]
rpt_thr<-argv[4]
contri<-argv[5]


#filter out the non-closed bugs, we only use closed bugs to conduct the experiment
#besides, we also removed the inactive reporter who totally reported less than rpt_thr, 
#ie. 10 closed bugs.
get_rpt_closed<-function(ori_rpt,rpt_thr){
   ori<-read.csv(ori_rpt,header=TRUE,sep=',')
   stat<-tolower(ori$bug_status)
   ori<-ori[stat=="closed" | stat=="resolved" | stat=="verified",]
   #get all closed_bugs,here C means closed
   ori_rpt_c<-paste(ori_rpt,'_C',sep='')
   write.table(ori,ori_rpt_c,sep=',',col.names=T,row.names=F)   
   r<-ddply(ori,.(reporter),summarize,cnt=length(bug_id))
   r<-r[as.numeric(r$cnt)>=as.numeric(rpt_thr),]
   ori<-ori[ori$reporter %in% r$reporter,]
   return(ori)
}

#get each contributor's contribution by year. without divided by products.
#note that the rpt file is all closed bugs containing file
total_contri<-function(rpt_fin,fix_fin,cmt_fin,rpt_thr,contri_fout){
    r<-get_rpt_closed(rpt_fin,rpt_thr)
    print('haha,get r')
    print(head(r))
    ryear<-ddply(r,.(reporter,year=substr(creation_ts,1,4)),summarize,rptCnt=length(bug_id),
                 BFR=length(resolution[tolower(resolution)=="fixed"])/length(resolution))
    names(ryear)<-c("who","when","rptCnt","BFR")    
#    ryear<-ryear[order(ryear$who,ryear$when),]
    ryear<-ddply(ryear,.(who),transform,rptCntPre=cumsum(rptCnt)-rptCnt)
    print(head(ryear))
    q()

    f<-read.csv(fix_fin,header=T,sep=",")
    fyear<-ddply(f,.(fixer,year=substr(fixTime,1,4)),summarize,fixCnt=length(bug_id))
    names(fyear)<-c("who","when","fixCnt")
    fyear<-fyear[order(fyear$who,fyear$when),]
    fyear<-ddply(fyear,.(who),transform,fixCntPre=cumsum(fixCnt)-fixCnt)

    c<-read.csv(cmt_fin,header=T,sep=",")
    cyear<-ddply(c,.(cmtor,year=substr(cmtTime,1,4)),summarize,cmtCnt=length(bug_id))
    names(cyear)<-c("who","when","cmtCnt")
    cyear<-cyear[order(cyear$who,cyear$when),]
    cyear<-ddply(cyear,.(who),transform,cmtCntPre=cumsum(cmtCnt)-cmtCnt)

#merge rptCnt,fixCnt and cmtCnt for each contributor by year
    rf<-merge(ryear,fyear,by=c("who","when"),all.x=T)
    rfc<-merge(rf,cyear,by=c("who","when"),all.x=T)
    write.table(rfc,file=contri,sep=",",col.names=T,row.names=F)
}

total_contri(ori_rpt,ori_fix,ori_cmt,rpt_thr,contri)

