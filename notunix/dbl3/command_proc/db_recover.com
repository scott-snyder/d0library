$!========================================================================
$!
$! Name      : DB_RECOVER
$!
$! Purpose   : use to recover from corruption
$!
$! Arguments : 
$!
$! Created  10-FEB-1992   SHAHRIAR ABACHI
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$ IF P1 .EQS. "" THEN -
   INQUIRE P1 " Which server (CA, MU, CD, FD, VT, TR, SA) ?"
$ IF (P1 .NES. "CA") .AND. (P1 .NES. "MU") .AND. (P1 .NES. "CD") -
 .AND. (P1 .NES. "FD") .AND. (P1 .NES. "VT") .AND. (P1 .NES. "TR") -
 .AND. (P1 .NES. "SA")
$ THEN
$  SAY " Subdetector selection parameter ''P1' is undefined"
$  GOTO EXIT
$ ENDIF
$!
$  DBID = P1
$!
$ IF (P1 .EQS. "CA") THEN -
   DBIDL = "CAL"
$ IF (P1 .EQS. "MU") THEN -
   DBIDL = "MUO"
$ IF (P1 .EQS. "CD") THEN -
   DBIDL = "CDC"
$ IF (P1 .EQS. "FD") THEN -
   DBIDL = "FDC"
$ IF (P1 .EQS. "VT") THEN -
   DBIDL = "VTX"
$ IF (P1 .EQS. "TR") THEN -
   DBIDL = "TRD"
$ IF (P1 .EQS. "SA") THEN -
   DBIDL = "SAM"
$!
$ WRITE SYS$OUTPUT "DETECTOR IS ''DBIDL'"
$!
$ DEFINE DBL3$CLIENT 'DBIDL'
$ FILE1 = "D0DB$''DBIDL':DBCALIB$''DBIDL'_START.DAT"
$ FILE2 = "D0DB$''DBIDL':DBCALIB$''DBIDL'_RECOVERED.DAT"
$ COPY 'FILE1' 'FILE2'
$!
$ DATABASE = "DBCALIB$''DBIDL'"
$ DEFINE  'DATABASE' 'FILE2'
$ JFILES = "''DBID'*.DBFZSENT"
$ IF F$SEARCH(JFILES) .EQS. "" THEN GOTO NEXT1
$ DIR 'JFILES'
$ COPY  'JFILES' *.DBRECOVER
$ NEXT1:
$ JFILES = "''DBID'*.DBFZNOTSENT"
$ IF F$SEARCH(JFILES) .EQS. "" THEN GOTO NEXT2
$ DIR 'JFILES'
$ COPY 'JFILES' *.DBRECOVER
$ NEXT2:
$ JFILES = "''DBID'*.DBFZCLOSE"
$ IF F$SEARCH(JFILES) .EQS. "" THEN GOTO NEXT3
$ DIR 'JFILES'
$ COPY 'JFILES' *.DBRECOVER
$ NEXT3:
$ JFILES = "''DBID'*.DBFZ_TOSEND"
$ IF F$SEARCH(JFILES) .EQS. "" THEN GOTO NEXT4
$ DIR 'JFILES'
$ COPY 'JFILES' *.DBRECOVER
$ NEXT4:
$!
$ RUN DBONLINE:[DBL3]DB_RECOVER/NODEB
$!
$ DEL 'DBID'*.DBRECOVER;*/NOCONFIRM
$!
$EXIT:
$   EXIT
