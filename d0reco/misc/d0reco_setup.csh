#!/bin/csh -f
#------------------------------------------------
#
# Name      : D0RECO_SETUP
#
# Purpose   : setup logicals and symbols for D0RECO program
#
# Arguments : $1 = task name
#             $2 = input file name
#             $3 = sta output area
#             $4 = dst output area
#             $5 = RCP type (DEFAULT, MC, COSMIC)
#
# Created  20-OCT-1989   Serban D. Protopopescu
# Updated  6-Apr-1994    Herbert Greenlee
#    Brought up to date with d0reco_setup.com.  Added RECO_TYPEs for
#    FT, showerlib.
#
#------------------------------------------------
set TASK = $1
set INPUT_DATA = $2
if( $INPUT_DATA == farm )then
  echo "Farm setup: d0reco framework links not modified"
  goto GET_RCP_TYPE
endif
set EXT_STA = 'sta'
set EXT_DST = 'dst'
if ( $INPUT_DATA == '' ) then
  d0echo "Give input data file : \c"
  set INPUT_DATA = `line | uff`
endif
if ( ! -f "$INPUT_DATA" ) goto NO_INPUT
if ( $INPUT_DATA !~ /* )set INPUT_DATA = $cwd/$INPUT_DATA
 
set DIRECTORY = $INPUT_DATA:h
set NAME = $INPUT_DATA:t
set NAME = $NAME:r
set EXT = $INPUT_DATA:e
 
if( `echo $EXT | tr '[a-z]' '[A-Z]'` == 'X_RAW01' )then
  set EXT_STA = 'x_sta01'
  set EXT_DST = 'x_dst01'
endif
 
if( "$EXT" == '' )then
  set evt = $DIRECTORY/${NAME}
else
  set evt = $DIRECTORY/${NAME}.$EXT
endif
ln -sf $evt event_data
 
set OUTPUT_DIR = $3
if ( $OUTPUT_DIR == '' ) then
  d0echo "Give STA output directory : \c"
  set OUTPUT_DIR = `line | uff`
endif
if ( ! -d "$OUTPUT_DIR" ) goto NO_OUTPUT
set DIRECTORY = $OUTPUT_DIR
 
set sta = $DIRECTORY/${NAME}.$EXT_STA
ln -sf $sta sta_output_data
 
set OUTPUT_DIR = $4
if ( $OUTPUT_DIR == '' ) then
  d0echo "Give DST output directory : \c"
  set OUTPUT_DIR = `line | uff`
endif
 
if ( ! -d "$OUTPUT_DIR" ) goto NO_OUTPUT
set DIRECTORY = $OUTPUT_DIR
 
set dst = $DIRECTORY/${NAME}.$EXT_DST
ln -sf $dst dst_output_data
 
set sum = $DIRECTORY/sum_${NAME}.out
ln -sf $sum summary_output
 
set hst = $DIRECTORY/${NAME}.hst4
ln -sf $hst histograms
 
ln -sf `uff $d0d0reco/d0reco.rcp` d0reco_rcp
 
GET_RCP_TYPE:
set RCP_TYPE = $5
if( "$RCP_TYPE" == '' )set RCP_TYPE = DEFAULT
set RCP_TYPE = `echo $RCP_TYPE | tr '[a-z]' '[A-Z]'`
 
GET_RECO_TYPE:
d0echo \
 "Using SD (Standard D0RECO) or FT (Full Tracking) or SH (SHowerlib)? [SD]: \c"
set RECO_TYPE = `line | tr '[a-z]' '[A-Z]'`
if( "$RECO_TYPE" == '' )set RECO_TYPE = SD
if( $RECO_TYPE == "SD" )goto DEFINE_RCP
if( $RECO_TYPE == "FT" )goto DEFINE_RCP
if( $RECO_TYPE == "SH" && $RCP_TYPE == "MC" )goto DEFINE_RCP
echo " Invalid RECO type "
goto GET_RECO_TYPE
 
DEFINE_RCP:
if( $RECO_TYPE == "SD" )echo " Setting RECO type to Standard D0RECO "
if( $RECO_TYPE == "FT" )echo " Setting RECO type to Full Tracking "
if( $RECO_TYPE == "SH" )echo " Setting RECO type to SHOWERLIB special "
if( $RECO_TYPE == "FT" && $RCP_TYPE == "DEFAULT" )then
  ln -sf `vff 'D0$CD_UTIL:FULL_ZTRAKS.RCP'` ztraks_rcp
  ln -sf `vff 'D0$FDC_UTIL:FULL_FTRAKS.RCP'` ftraks_rcp
endif
if( $RECO_TYPE == "FT" && $RCP_TYPE == "MC" )then
  ln -sf `vff 'D0$CD_UTIL:MC_FULL_ZTRAKS.RCP'` ztraks_rcp
  ln -sf `vff 'D0$FDC_UTIL:MC_FULL_FTRAKS.RCP'` ftraks_rcp
endif
if( $RECO_TYPE == "FT" && $RCP_TYPE == "COSMIC" )then
  ln -sf `vff 'D0$CD_UTIL:COSMIC_FULL_ZTRAKS.RCP'` ztraks_rcp
  ln -sf `vff 'D0$FDC_UTIL:COSMIC_FULL_FTRAKS.RCP'` ftraks_rcp
endif
if( $RECO_TYPE == "SH" && $RCP_TYPE == "MC" )then
#  ln -sf `vff 'D0$CALOR_OFF:CAHITS_MC_VERTEX.RCP'` cahits_rcp
#  ln -sf `vff 'D0$CALOR_OFF:CAPHEL_MC_VERTEX.RCP'` caphel_rcp
#  ln -sf `vff 'D0$MUON_RECO:MC_MURECO_MC_VERTEX.RCP'` mureco_rcp
  ln -sf `vff 'd0$cd_util:vertex_isajet.rcp'` vertex_rcp
endif
 
if( $INPUT_DATA != farm )then
  echo
  echo "Input data file is : $evt"
  echo "STA output file is : $sta"
  echo "DST output file is : $dst"
  echo "Histogram  file is : $hst"
  echo "Summary    file is : $sum"
endif
EXIT:
exit
 
NO_INPUT:
/bin/echo  "No input file was defined."
 
NO_OUTPUT:
/bin/echo  "No output directory was defined."
/bin/echo  "You must give an input file and output directories."
exit
