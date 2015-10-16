#!/bin/bash
#get the rpt|fix|cmt contri
Rscript --slave final_get_contri.R rptInfo_E fixInfo_E cmtInfo_E 10 E > run_E.log 2 >&1&
Rscript --slave final_get_contri.R rptInfo_M fixInfo_M cmtInfo_M 10 M > run_M.log 2>&1&

#merge the rpt|fix|cmt contri
Rscript --slave merge_contri_year.R rptInfo_E_BFR E_rpt E_fix E_cmt E
Rscript --slave merge_contri_year.R rptInfo_M_BFR M_rpt M_fix M_cmt M

#cal correlation between BFR and kinds of contributions(rpt|fix|cmt)
Rscript --slave pre_cur_Cnt_BFR_corr.R E_BFR_rfc > allContri_BFR_corr_E.res
Rscript --slave pre_cur_Cnt_BFR_corr.R M_BFR_rfc > allContri_BFR_corr_M.res

Rscript --slave latestContri_corr.R E_BFR_rfc 10 > allContri_BFR_corr_E_maxActiveYear.res
Rscript --slave latestContri_corr.R M_BFR_rfc 10 > allContri_BFR_corr_M_maxActiveYear.res

