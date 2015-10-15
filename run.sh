#!/bin/bash

Rscript --slave final_get_contri.R rptInfo_E fixInfo_E cmtInfo_E 10 E > run_E.log 2 >&1&
Rscript --slave final_get_contri.R rptInfo_M fixInfo_M cmtInfo_M 10 M > run_M.log 2>&1&
