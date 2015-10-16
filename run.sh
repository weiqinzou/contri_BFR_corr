#!/bin/bash
#get the rpt|fix|cmt contri
Rscript --slave final_get_contri.R rptInfo_E fixInfo_E cmtInfo_E 10 E > run_E.log 2 >&1&
Rscript --slave final_get_contri.R rptInfo_M fixInfo_M cmtInfo_M 10 M > run_M.log 2>&1&

#merge the rpt|fix|cmt contri
Rscript --slave merge_contri_year.R rptInfo_E_BFR E_rpt E_fix E_cmt E
Rscript --slave merge_contri_year.R rptInfo_M_BFR M_rpt M_fix M_cmt M
