$!========================================================================
$!
$! Name      : FM_LIST
$!
$! Purpose   : Make a list of FATMEN generic names for specified data type
$!             and run numbers, in appropriate format for FILE_NAMES.
$!
$! Arguments : P1      blank (give instructions) or T (omit instructions)
$!             P2      run number(s) [no default] e.g. 057*, 58886, *, ...
$!             P3      data type: DST,STA,RAW  [DST]
$!             P4      input stream  EXP,ALL,...  [ALL]
$!             P5      trigger stream  ALL,MUF,MET,MIN,JET  [ALL]
$!             P6      filter stream NONE,RGE,QCD  [RGE]
$!             P7      cataloged since this date [000000]
$!                       Format = YYMMDD (e.g. 930131)
$!             P8      cataloged before this date [931231]
$!                       Format = YYMMDD (e.g. 930131)
$!
$! Created   5-FEB-1993   K. Wyatt Merritt
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WR :== WRITE SYS$OUTPUT
$   WF :== WRITE file
$   WF2 :== WRITE file2
$ IF P1 .NES. "T" 
$ THEN
$    WR " Specify run numbers as, e.g.,  057*,0571*, or 057686*: " 
$    WR " Always use the initial zero, and a minimum of 3 digits."
$    WR " If your data type is STA or RAW, the search will take a"
$    WR " VEERRRY long time if you specify many run numbers.     "
$    WR " You can also add the partition specifier at the end of "
$    WR " the run number (e.g. 057*P01 to get the first partition"
$    WR " of all 57000 runs), but it must be appropriate to the  "
$    WR " data type and stream."
$ ENDIF
$ GETRUN:
$   IF P2 .EQS. "" THEN INQUIRE P2 " Run numbers to search for "
$   IF P2 .EQS. "" THEN GOTO GETRUN
$   P2A = F$EXTRACT(0,3,P2)
$!
$ IF P1 .NES. "T" 
$ THEN
$    WR " Allowed data types are DST, STA, or RAW."
$ ENDIF
$   IF P3 .EQS. "" THEN INQUIRE P3 " Data type [DST] "
$   IF P3 .EQS. "" THEN P3 = "DST"
$ IF P1 .NES. "T" 
$ THEN
$    WR " The input stream is ALL or EXP, except for special runs."
$ ENDIF
$   IF P4 .EQS. "" THEN INQUIRE P4 " Input stream [ALL] "
$   IF P4 .EQS. "" THEN P4 = "ALL"
$ IF P1 .NES. "T" 
$ THEN
$    WR " Trigger streams are ALL,ELF,MUF,JET,MET, or MIN."
$ ENDIF
$   IF P5 .EQS. "" THEN INQUIRE P5 " Trigger stream [ALL] "
$   IF P5 .EQS. "" THEN P5 = "ALL"
$ IF P1 .NES. "T" 
$ THEN
$    WR " Filter streams are NONE,RGE, or QCD."
$ ENDIF
$   IF P6 .EQS. "" THEN INQUIRE P6 " Filter stream [RGE] "
$   IF P6 .EQS. "" THEN P6 = "RGE"
$ WR "  Date format = YYMMDD (e.g. 930131 for 31 Jan 1993)"
$   IF P7 .EQS. "" THEN INQUIRE P7 " Cataloged since date [000000]"
$   IF P7 .EQS. "" THEN P7 = "000000"
$   IF P8 .EQS. "" THEN INQUIRE P8 " Cataloged before date [931231]"
$   IF P8 .EQS. "" THEN P8 = "931231"
$ IF P3 .NES. "RAW" 
$ THEN
$    INQUIRE MACH " RECO from VAX or UNIX [UNIX]" 
$    IF MACH .EQS. ""     THEN VER = "REU"
$    IF MACH .EQS. "UNIX" THEN VER = "REU"
$    IF MACH .EQS. "VAX"  THEN VER = "REC"
$ ELSE
$    VER = "DAQ"
$ ENDIF
$   COPY NL: FM_LIST.KUMAC
$   OPEN/APPEND file FM_LIST.KUMAC
$!
$ WF "MESSAGE ' Executing FM_LIST.KUMAC...'"
$ WF "CD //FNAL/D0/CLDA/92P1E18"
$ WF "LOGL -1"
$ WF "SEARCH ''P3'%%''VER'%%%%/''P4'*''P5'*''P6'/''P2A*/R''P2' _"
$ WF " CATALOGED=''P7'-''P8' -G OUTPUT=FMLIST.TMP"
$!
$   CLOSE file
$ INQUIRE ANS " Submit FM query as a batch job? "
$ IF ANS .EQS. "Y" THEN GOTO BATCH
$ FM
EXEC FM_LIST
MESSAGE ' End execution of FM_LIST.KUMAC.'
EXIT
$ WR "  "
$ WR " Formatting and sorting the search result..."
$ SEARCH/OUT=FMLIST.TMP2 FMLIST.TMP "//FNAL"
$ SORT/NODUP/KEY=(POS:61,SIZ:9) FMLIST.TMP2 FMLIST.GEN
$ DELETE/NOCONF FMLIST.TMP;*,FMLIST.TMP2;*,FM_LIST.KUMAC;*,LAST.KUMAC.
$ WR "  "
$ WR " The FATMEN generic names appear in FMLIST.GEN."
$ WR "  "
$ GOTO EXIT
$BATCH:
$   COPY NL: FM_BATCH.COM
$   OPEN/APPEND file2 FM_BATCH.COM
$!
$ WF2 "$SET DEF ''CUR_DIR'"
$ WF2 "$FM"
$ WF2 "EXEC FM_LIST"
$ WF2 "MESSAGE 'End execution of FM_LIST.KUMAC'"
$ WF2 "EXIT"
$ wf2 "SEARCH/OUT=FMLIST.TMP2 FMLIST.TMP ""//FNAL"""
$ WF2 "SORT/NODUP/KEY=(POS:61,SIZ:9) FMLIST.TMP2 FMLIST.GEN"
$ WF2 "DELETE/NOCONF FMLIST.TMP;*,FMLIST.TMP2;*,FM_LIST.KUMAC;*,LAST.KUMAC."
$!
$   CLOSE file2
$ SUBMIT/NAME=FMBATCH/CHAR=(D0GS01) FM_BATCH.COM
$ !DELETE/NOCONF FM_BATCH.COM;*
$EXIT:
$   EXIT
