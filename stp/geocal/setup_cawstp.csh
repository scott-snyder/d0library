#!/bin/csh -f
#------------------------------------------------
#
# Name      : SETUP_CAWSTP.COM
#
# Purpose   : Setup logical names and commands for
#             programs which create the calorimeter
#             geometrical data in GEANT format.
#
# Created  10-FEB-1989   Harrison B. Prosper
#
#------------------------------------------------
#
set INPUT = "$d0stp/geocal/"
set OUTPUT = ""
#
#------------------------------------------------
#   Define INPUT files
#------------------------------------------------
#
ln -sf `uff ${INPUT}cawstp.rcp` cawstp_rcp >& /dev/null
ls -l cawstp_rcp
#
ln -sf `uff ${INPUT}srcp_raw_ec.dat` srcp_raw_ec >& /dev/null
ls -l srcp_raw_ec
#
ln -sf `uff ${INPUT}srcp_raw_cc.dat` srcp_raw_cc >& /dev/null
ls -l srcp_raw_cc
#
ln -sf `uff ${INPUT}srcp_raw_cry.dat` srcp_raw_cry >& /dev/null
ls -l srcp_raw_cry
#
ln -sf `uff ${INPUT}srcp_raw_icd.dat` srcp_raw_icd >& /dev/null
ls -l srcp_raw_icd
#
ln -sf `uff ${INPUT}srcp_raw_lv0.dat` srcp_raw_lv0 >& /dev/null
ls -l srcp_raw_lv0
#
ln -sf `uff ${INPUT}srcp_raw_rest.dat` srcp_raw_rest >& /dev/null
ls -l srcp_raw_rest
#
ln -sf `uff ${INPUT}cc_survey.rcp` cc_survey_rcp >& /dev/null
ls -l cc_survey_rcp
#
ln -sf `uff ${INPUT}ecn_survey.rcp` ecn_survey_rcp >& /dev/null
ls -l ecn_survey_rcp
#
ln -sf `uff ${INPUT}ecs_survey.rcp` ecs_survey_rcp >& /dev/null
ls -l ecs_survey_rcp
#
ln -sf `uff ${INPUT}cal_survey_markers.rcp` cal_survey_markers_rcp >& /dev/null
ls -l cal_survey_markers_rcp
#
echo
#------------------------------------------------
#   Define OUTPUT files
#------------------------------------------------
#
ln -sf ${OUTPUT}srcp_ecal.dat srcp_ecal >& /dev/null
ls -l srcp_ecal
#
ln -sf ${OUTPUT}srcp_ucal.dat srcp_ucal >& /dev/null
ls -l srcp_ucal
#
ln -sf ${OUTPUT}srcp_rest.dat srcp_rest >& /dev/null
ls -l srcp_rest
#
ln -sf ${OUTPUT}srcp_lv0.dat srcp_lv0 >& /dev/null
ls -l srcp_lv0
#
ln -sf ${OUTPUT}cal_stpfile.dat cal_stpfile >& /dev/null
ls -l cal_stpfile
#
ln -sf ${OUTPUT}lv0_stpfile.dat lv0_stpfile >& /dev/null
ls -l lv0_stpfile
#
ln -sf ${OUTPUT}cad_stpfile.dat cad_stpfile >& /dev/null
ls -l cad_stpfile
#
ln -sf ${OUTPUT}ccpt_stpfile.dat ccpt_stpfile >& /dev/null
ls -l ccpt_stpfile
#
ln -sf ${OUTPUT}ccpc_stpfile.dat ccpc_stpfile >& /dev/null
ls -l ccpc_stpfile
#
ln -sf ${OUTPUT}ccua_stpfile.dat ccua_stpfile >& /dev/null
ls -l ccua_stpfile
