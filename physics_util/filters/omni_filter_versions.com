$!========================================================================
$!
$! Name      : OMNI_FILTER_SETUP
$!
$! Purpose   : setup the stream definitions and versions for omni_filter
$!
$! Arguments : 
$!
$! Created  16-APR-1993   Lee Lueking
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!========================================================================
$!             Stream and filter version AND definitions:
$!========================================================================
$   MIN_VSN=="20"    
$   MIN_DEF=="STREAM"
$   QC2_VSN=="20"
$   QC2_DEF=="STREAM"
$   QC1_VSN=="20"
$   QC1_DEF=="STREAM"
$   ELF_VSN=="20"    
$   ELF_DEF=="STREAM"
$   MUF_VSN=="20"
$   MUF_DEF=="STREAM"
$   MU1_VSN=="20"
$   MU1_DEF=="STREAM"
$   MET_VSN=="20"
$   MET_DEF=="STREAM"
$   RGE_VSN=="20"
$   RGE_DEF=="FILTER"
$   WZS_VSN=="20"
$   WZS_DEF=="FILTER"
$   TOP_VSN=="20"
$   TOP_DEF=="FILTER"
$   FAK_VSN=="20"
$   FAK_DEF=="FILTER"
$   WZE_VSN=="20"
$   WZE_DEF=="FILTER"
$   WZM_VSN=="20"
$   WZM_DEF=="FILTER"
$   NPL_VSN=="20"
$   NPL_DEF=="FILTER"
$   NPH_VSN=="20"
$   NPH_DEF=="FILTER"
$   QGA_VSN=="20"
$   QGA_DEF=="FILTER"
$   B2M_VSN=="20"
$   B2M_DEF=="FILTER"
$   BSM_VSN=="20"
$   BSM_DEF=="FILTER"
$   B2E_VSN=="20"
$   B2E_DEF=="FILTER"
$   WZP_VSN=="20"
$   WZP_DEF=="FILTER"
$   NPX_VSN=="20"
$   NPX_DEF=="FILTER"
$   TMJ_VSN=="20"
$   TMJ_DEF=="FILTER"
$EXIT:
$   EXIT
