$!========================================================================
$!
$! Name      : MERGE2
$!
$! Purpose   : SECOND LEVEL MERGING.
$!
$! Arguments : 
$!
$! Created  14-SEP-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$DEFINE BETA D0$CMS:[SHOWERLIBRARY]
$SET DEF D0$CMS:[SHOWERLIBRARY]
$@BETA:SETUP_SHOWERLIBRARY BLND15
$SET DEF UF13:
$ ARG = "MERGESORT_BLND15_UNSORT.DAT019,"+-
        "MERGESORT_BLND15_UNSORT.DAT1019,"+-
        "MERGESORT_BLND15_UNSORT.DAT2029,"+-
        "MERGESORT_BLND15_UNSORT.DAT3039,"+-
        "MERGESORT_BLND15_UNSORT.DAT4049,"+-
        "MERGESORT_BLND15_UNSORT.DAT5059,"+-
        "MERGESORT_BLND15_UNSORT.DAT6069,"+-
        "MERGESORT_BLND15_UNSORT.DAT7074"
$SHO SYM ARG
$   MERGE/KEY=(POSITION:1,SIZE:4,BINARY)/STATISTICS  'ARG'-
 UF13:MERGE_ALL_BLND15.DAT
$EXIT:
$   EXIT
