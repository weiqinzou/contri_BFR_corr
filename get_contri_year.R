library('plyr',lib.loc="~/myOwnRPackage")

#run example:
#Rscript --slave get_contri_year.R rptInfo_M fixInfo_M cmtInfo_M 10 contri_rfc_M&
#Rscript --slave get_contri_year.R rptInfo_E fixInfo_E cmtInfo_E 10 contri_rfc_E&

argv<-commandArgs(TRUE)
ori_rpt<-argv[1]
ori_fix<-argv[2]
ori_cmt<-argv[3]
rpt_thr<-argv[4]


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

preContri<-function(contri_eachyear_d,contri_fout){
#contri_eachyear_d is a dataframe which contains such items:bug_id,who,when
    contri_eachyear_d<-ddply(contri_eachyear_d,.(who,year=substr(when,1,4)),
                             summarize,cnt=length(bug_id))
    names(contri_eachyear_d)<-c("who","when","cnt")
    contributors<-unique(contri_eachyear_d$who)
#clear the contri_fout and write the head into the contri_fout before for loop
    head<-c("who","when","cnt","cntPre\n")
    cat(head,file=contri_fout,sep=",",append=F)

    for(c in contributors){
        contri<-contri_eachyear_d[contri_eachyear_d$who==c,]
        contri<-contri[order(contri$who,contri$when),]
        contri<-ddply(contri,.(who),transform,cntPre=cumsum(cnt)-cnt)
        write.table(contri,contri_fout,sep=",",row.names=F,col.names=F,append=T)
        gc()
    }
}

fixcmtor_preContri<-function(fixcmt_d,activeRptor,contri_fout){
    names(fixcmt_d)<-c("bug_id","who","when")
#filter the fixer or cmtor who are not within the active reporters.
    fixcmt_d<-fixcmt_d[fixcmt_d$who %in% activeRptor,]
    preContri(fixcmt_d,contri_fout)
}

get_rptor_BFR<-function(rpt_d,BFR_fout){
#rpt_d is a dataframe which contains:bug_id,reporter,resolution,creation_ts
    rd<-ddply(rpt_d,.(reporter,year=substr(creation_ts,1,4)),summarize,
          BFR=length(resolution[tolower(resolution)=="fixed"])/length(resolution))
    names(rd)<-c("who","when","BFR")
    write.table(rd,BFR_fout,sep=",",col.names=T,row.names=F) 
}
#get each contributor's contribution by year. without divided by products.
#note that the rpt file is all closed bugs containing file
total_contri<-function(rpt_fin,fix_fin,cmt_fin,rpt_thr){
    r<-get_rpt_closed(rpt_fin,rpt_thr)
    r<-r[,c("bug_id","reporter","creation_ts","resolution")]
    get_rptor_BFR(r,paste(rpt_fin,'BFR',sep='_'))

    r<-r[,-4]
    names(r)<-c("bug_id","who","when")
    preContri(r,paste(rpt_fin,'preCnt',sep='_'))

    p<-unique(r$who)
    f<-read.csv(fix_fin,header=T,sep=",")
    fixcmtor_preContri(f,p,paste(fix_fin,'preCnt',sep='_'))   
    
    c<-read.csv(cmt_fin,header=T,sep=",")
    fixcmtor_preContri(c,p,paste(cmt_fin,'preCnt',sep='_'))   
}


total_contri(ori_rpt,ori_fix,ori_cmt,rpt_thr)

