$!========================================================================
$!
$! Name      : OMNI_FILTER_SETUP
$!
$! Purpose   : setup for omni_filter projects
$!
$! Arguments : P1 project code: EXP, STA, DST, RGE
$!             P2 RECO version: If .lt. 1012 do emfix
$!
$! Created  10-MAY-1993   Lee
$!
$!========================================================================
$PC==P1	!PROJECT CODE
$RV=="" !RECO VERSION
$D0RECO:=="RUN/NODEBUG PROD$OMNI_FILTER:OMNI_FILTER_D0RECO"
$!IF F$INTEGER(P2).GE.1012 THEN RV=="_GE1012"
$!
$! DEFINITIONS FOR SPECIAL RCP FILES
$!
$DEFINE UNI_FILTER_RCP    prod$omni_filter:STREAM_FILTER_'PC''RV'.RCP
$DEFINE MULTI_FILTER_RCP  prod$omni_filter:STREAM_FILTER_'PC''RV'.RCP
$DEFINE STREAM_FILTER_RCP prod$omni_filter:STREAM_FILTER_'PC''RV'.RCP
$define strip_rcp d0$physics_util:strip_mu1b.rcp !temp until next release
$show log prod$omni_filter
$show log *_rcp
$EXIT
