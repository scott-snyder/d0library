$!========================================================================
$!
$! Name      : Convert
$!
$! Purpose   : Converts a sorted file to a Keyd access file
$!
$! Arguments : P1 = Input file name
$!             P2 = Output file name
$!             P3 = FDL_NAME
$!
$! Created  31-AUG-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$!DEFINE BETA D0$CMS:[SHOWERLIBRARY]
$!DEFINE BETA usr$root1:[SINGHJB.CALOR_OFF.MUON]
$!SET DEF D0$CMS:[SHOWERLIBRARY]
$!@BETA:SETUP_SHOWERLIBRARY BLND14
$!@SETUP_SHOWERLIBRARY BLND14
$!@SETUP_MUONLIBRARY BLND14
$! SET DEF SF13:
$!define sys$output  convert.dat
$ IF P1 .EQS. "" THEN P1 = "UF13:MERGE_BLND151614.DAT"
$ IF P2 .EQS. "" THEN P2 = "BLND151614_SHOWERLIBRARY.DAT"
$ IF P3 .EQS. "" THEN P3 = "BETA:BLND151614"
$
$ SHOW SYM P1
$ SHOW SYM P2
$ SHOW SYM P3
$
$ FDL_NAME = P3
$ WRITE SYS$OUTPUT "Start - " + F$TIME()
$ CONVERT/FAST_LOAD/FDL='FDL_NAME'.FDL/FILL_BUCKETS/NOSORT/CREATE/STATISTICS -
   'P1' 'P2'
$ WRITE SYS$OUTPUT "End - " + F$TIME()
$ WRITE SYS$OUTPUT "Start - " + F$TIME()
$!@beta:analyze_rms   'p2'
$ WRITE SYS$OUTPUT "End - " + F$TIME()
$EXIT:
$   EXIT
