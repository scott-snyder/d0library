$!========================================================================
$!
$! Name      : RESTORE_RUN
$!
$! Purpose   : Restore files for a run from copycfg$archive for vms_filter
$!              Run on D0 online cluster, or define copycfg$archive yourself
$!
$! Arguments : P1   run number (5 digits; will break at run 100K)
$!
$! Created  23-MAR-1994   L2_MON
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$  
$! Ascii version of filter parameters, just for browsing by humans
$ ZEROS := "00"
$ IF (P1.GE.100000) THEN ZEROS := "0"
$ COPY COPYCFG$ARCHIVE:RCP_'zeros''P1'.DAT RCP_0000001.DAT
$!  filter parameter sets
$ COPY COPYCFG$ARCHIVE:RCP_'zeros''P1'.ZDAT RCP_0000001.ZDAT
$!  filter script
$ COPY COPYCFG$ARCHIVE:RUN_FILTER_'zeros''P1'.DAT RUN_FILTER_0000001.DAT
$!  trigger and filter names
$ COPY COPYCFG$ARCHIVE:TRIG_FILT_'zeros''P1'.INFO TRIG_FILT_RUN.INFO
$ SWAP := $d0$util:swap
$ SWAP TRIG_FILT_RUN.INFO 'P1' "00000"
$EXIT:
$   EXIT
