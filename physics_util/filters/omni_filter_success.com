$!========================================================================
$!
$! Name      : OMNI_FILTER_SUCCESS
$!
$! Purpose   : OMNI_FILTER successful completion command procedure for use with 
$!             production manager
$!
$!             The symbols DST_STREAMS and STA_STREAMS determine the 
$!             output files which are treated. (currently they do not work together)
$!             THEY ARE DEFINED IN OMNI_setup.COM
$!
$!                  DST_STREAMS=1 handle dst streams
$!                  DST_STREAMS=0 don't handle dst streams
$!                  STA_STREAMS=1 handle sta streams
$!                  STA_STREAMS=0 don't handle sta streams
$!
$! Created  11-SEP-1992  Lee Lueking
$! Modified 10-MAR-1993  Lee Lueking Supports variable stream number and definitions
$! Modified 21-SEP-1993   Dorota I. Genser - changes for NEW JOBMAN
$!
$!========================================================================
$!  Stream and filter version AND definitions are setup in 
$!  omni_filter_versions.com MOST SPECS ARE SETUP IN OMNI_SETUP.COM
$!========================================================================
$!------------------------------------------------
$!Find cpu and elapsed clock time
$!------------------------------------------------
$ @pm_exe$dir:elapsed_time "''start_reco_time'"
$ wr ""
$ wr " Elapsed time ''clock_time' Clock time ''cpu_time'"
$ wr ""
$!------------------------------------------------
$!   get the 7 CHAR TIME STAMP
$!------------------------------------------------
$ CURRENT_DATE==f$time()
$ @PM_EXE$DIR:TIME_STAMP
$ wr ""
$!------------------------------------------------
$!   get the numbers from the d0reco status file 
$!------------------------------------------------
$ @pm_exe_directory:pm_INFO job_status
$ wr ""
$ TAPEVSN=F$EXTRACT(0,6,INPLIST)
$!------------------------------------------------
$! IF  the "complete" summary file does not exist, open it
$!------------------------------------------------
$ SUM_FILE="PM_LOG$DIR:''PROJECT_NAME'_SUMMARY_''F$extract(0,5,time_stamp)'.DAT"
$ IF F$SEARCH(SUM_FILE).EQS."" 
$   THEN
$   OPEN/WRITE/share SUMMARY 'SUM_FILE'
$   WS:=="WRITE SUMMARY"
$   WS "!--------------------------------------------------------------------!"
$   WS "! ProMan summary file created ''f$time()'        !"
$   WS "! * means beginning of new entry, + means continuation of entry      !"
$   WS "! Legend:                                                            !"
$   WS "! * QID  PROJECT_NAME    NODE   #EVENTS CLKTIM CPUTIM  YMMDDHH         !"
$   WS "! + QID i TAPE    INPUT_FILE_1.SPEC                                  !"
$   WS "! + QID i TAPE    INPUT_FILE_2.SPEC                                  !"
$   WS "! + QID o NOUT_EV OUTPUT_FILE_1.SPEC                                 !"
$   WS "! + QID o NOUT_EV OUTPUT_FILE_2.SPEC                                 !"
$   WS "! + QID o NOUT_EV OUTPUT_FILE_3.SPEC                                 !"
$   WS "!--------------------------------------------------------------------!"
$   CLOSE SUMMARY
$   ENDIF
$!------------------------------------------------
$! append the job information to the "complete" summary file
$!------------------------------------------------
$NQID=F$INTEGER(QID)
$NEP =F$INTEGER(EVENTS_PROCESSED)
$ OPEN/APPEND/SHARE=WRITE SUMMARY 'SUM_FILE'
$ SUMMARY_LINE = F$FAO("!8SL!1AS!20AS!8AS!3(8SL)!1AS!7AS",-
    NQID," ",PROJECT_name,NODE,NEP,CLOCK_TIME,CPU_TIME," ",TIME_STAMP)
$ WRITE SUMMARY "*''SUMMARY_LINE'" 
$!
$!------------------------------------------------
$! GENERAL STUFF FOR all RCPS
$!------------------------------------------------
$ RAW_STREAM=F$EXTRACT(0,3,INPFILE)
$ DEFINE/nolog RCPFILE PM$RUN:'jobname'.rcp
$pm_MAKE_RCP/OUTRCP=RCPFILE/BANK_NAME=FILE_INFO_RCP-
/PRODUCTION_NODE='NODE'/CALIB_DATA_FILE=UNDEFINED-
/STREAM_NAME='RAW_STREAM'-
/PRODUCTION_PASS_NUM=%1/PRODUCTION_DATE='CURRENT_DATE'
$!
$PM_MAKE_RCP/INRCP=RCPFILE/BANK_NAME=FILE_INFO_RCP-
/FIRST_EVENT=%'FIRST_EVENT'/LAST_EVENT=%'LAST_EVENT'-
/FIRST_RUN=%'FIRST_RUN'/LAST_RUN=%'LAST_RUN'-
/EVENTS_PROCESSED=%'EVENTS_PROCESSED'-
/PRODUCTION_VERSION_NUM=-
('PROGRAM_NAME' V'PROGRAM_VERSION'.'PROGRAM_PASS')-
/PM_STATUS=SUCCESS
$!
$PM_MAKE_RCP/INRCP=RCPFILE/BANK_NAME=FILE_INFO_RCP-
/DATA_REPRESENTATION=%3/RECORD_FORMAT=RMS/RECORD_LENGTH=%8190/BLOCK_LENGTH=%8190
$!
$pm_MAKE_RCP/INRCP=RCPFILE/BANK_NAME=FILE_INFO_RCP-
/HOST_NAME=D0FSA/FILE_LOCATION=%100/HOST_PLATFORM=VAX/HOST_OP_SYSTEM=VMS
$!
$pm_MAKE_RCP/INRCP=RCPFILE/BANK_NAME=FILE_INFO_RCP-
/OWNER=FATMEN/CREATOR_NAME=D0RECO/CREATOR_PROCESS=(PROMAN)
$!------------------------------------------------
$!  need input list of files
$!------------------------------------------------
$  NFILES=1
$  first_part = "00"
$  last_part  = "00"
$  OPEN/READ INPUTS 'INPDIR''INPLIST'
$  DOT_OUT_FILE="''tape_name'.OUT"
$  OPEN/WRITE OUT_FILE 'DOT_OUT_FILE'
$  WRITE OUT_FILE "!------------------------------------------------------------"
$  LOOP_FILES:
$    READ/END_OF_FILE=EOF INPUTS LINE
$    WRITE OUT_FILE "! ''LINE' "
$    SUMMARY_LINE = F$FAO("!8SL!4AS!6AS",NQID," i  ",TAPEVSN)+" ''line'"
$    WRITE SUMMARY "+''SUMMARY_LINE'" 
$    pi = F$parse(line,,,"name")
$    pi = F$extract(F$length(pi)-2,2,pi)
$    IF NFILES .EQ. 1 
$    THEN 
$      INPFILE="''LINE'"
$      pm_MAKE_RCP/INRCP=RCPfile/BANK_NAME=FILE_INFO_RCP/INPUT_FILENAME='LINE'
$      first_part = pi
$    ELSE
$      pm_MAKE_RCP/INRCP=RCPfile/BANK_NAME=FILE_INFO_RCP/ADD/INPUT_FILENAME='LINE'
$      last_part = pi
$    ENDIF
$    NFILES=NFILES+1
$  GOTO LOOP_FILES
$  EOF:
$  CLOSE INPUTS
$  WRITE OUT_FILE "!------------------------------------------------------------"
$  CLOSE OUT_FILE
$!
$pm_MAKE_RCP/INRCP=RCPFILE/BANK_NAME=FILE_INFO_RCP-
  /COMMENT=( STREAMING ON D0FS partitions range 'first_part'-'last_part' )
$!
$  append/nolog 'DOT_OUT_FILE';-1 'DOT_OUT_FILE'
$  edt  'DOT_OUT_FILE'
exit
$  purge/nocon/nolog 'DOT_OUT_FILE' 
$  rename/nolog 'DOT_OUT_FILE' PM$SUMMARY:'Filname'.OUT_'project_code'
$  WR " OUT file has been renamed to ''Filname'.OUT_''project_code' "
$!
$ purge/nocon/nolog RCPFILE
$!
$!------------------------------------------------
$! Loop over the streams, make an RCP file for each, 
$! remove _LOCK and rename files, move DSTs to 
$! buffer area.
$!------------------------------------------------
$!
$ NAME=F$PARSE(STAFILE,,,"NAME")
$ TYPE=F$PARSE(STAFILE,,,"TYPE")
$!------------------------------------------------
$!Get the RECO VERSION NUMBER
$!------------------------------------------------
$ IF F$EXTRACT(8,2,TYPE) .EQS. "RE" ! New standard file name
$  THEN
$  VREC=F$EXTRACT(11,4,TYPE)
$  IF RECO_TYPE .EQS. "" 
$    THEN
$     RECO=F$EXTRACT(8,3,TYPE)
$    ELSE
$     RECO="''RECO_TYPE'" !This indicates SPECIAL fix has been done
$    ENDIF
$  ELSE
$   VREC="0905"
$   RECO="REU"
$  ENDIF
$ LEN=F$LENGTH(NAME)
$ NEW_NAME=F$EXTRACT(0,LEN-3,NAME)+"''MERGE_TYPE'"+F$EXTRACT(LEN-2,2,NAME) !PUT MERGE TYPE
$ RUN_NUMBER=F$EXTRACT(LEN-9,6,name)
$ RUN_DIRE="000000''F$integer( (RUN_NUMBer/1000)*100)'"
$ RUN_DIRE = F$extract(F$length(RUN_DIRE)-5,5,RUN_DIRE)
$ PARTITION=F$EXTRACT(LEN-2,2,name)
$ RAW_STREAM=f$extract(0,3,stafile)
$ NSTREAMS=F$INTEGER(STREAMS)
$ NSTR=0
$!
$STREAM_LOOP:
$!-----------
$ NSTR=NSTR+1
$ WR ""
$!
$ IF NSTR.GT.NSTREAMS THEN GOTO STREAM_END
$ WR " NSTR= ''NSTR' NSTREAMS= ''NSTREAMS'"
$!
$ Snstr = "''nstr'" 
$ IF NSTR.LT.10 THEN Snstr = "0''nstr'"
$ STREAM_NAME=F$EDIT(STREAM_NAME_'Snstr',"COLLAPSE")
$ STREAM_DEF ='STREAM_NAME'_DEF
$ STREAM_VSN ='STREAM_NAME'_VSN
$ STREAM_OUT ='STREAM_NAME'_OUT
$ WR "STREAM_DEF: ''STREAM_DEF', STREAM_VSN: ''STREAM_VSN'"
$!
$ STREAM_TYPE   =F$EDIT(STREAM_TYPE_'Snstr',"COLLAPSE")
$ STREAM_EVENTS =F$EDIT(STREAM_EVENTS_'Snstr',"COLLAPSE")
$!
$!------------------------------------------------
$! FOR SOME REASON, EVEN WITH STA'S OFF, THERE ARE
$! 20 STREAMS. MUST CHECK STREAM TYPE AND SKIP STA'S
$!------------------------------------------------
$IF (STA_STREAMS) .AND. (STREAM_TYPE .EQS. "DST") THEN GOTO STREAM_LOOP
$IF (DST_STREAMS) .AND. (STREAM_TYPE .EQS. "STA") THEN GOTO STREAM_LOOP
$!
$!------------------------------------------------
$! In the omni-filter job, there are 6 streams and 
$! 4 filter outputs. They must be sorted out here.
$!------------------------------------------------
$ STREAM_NAME_OLD=STREAM_NAME+"01"
$ IF (STREAM_DEF .EQS. "FILTER")
$  THEN
$    FILTER_NAME="''STREAM_NAME'XX''STREAM_VSN'" 
$    STREAM_NAME="ALL00" !THIS WILL HAVE TO CHANGE 
$                      !IF FILTERS ARE RUN ON STREAMS
$  ELSE
$    FILTER_NAME="NONEX00"
$    STREAM_NAME="''STREAM_NAME'''STREAM_VSN'"
$  ENDIF
$!------------------------------------------------
$! Modify the output FILE NAMES
$! MAKE THE RCP FILE NAMES
$!------------------------------------------------
$ NEW_TYPE="''TYPE'"
$ TMP_TYPE="''TYPE'"
$ IF F$LENGTH(TYPE).LT.9 THEN NEW_TYPE="''TYPE'REC''VREC'"
$ NEW_TYPE[3,3]:="''STREAM_TYPE'"
$ NEW_TYPE[8,3]:="''RECO_TYPE'"
$ TMP_TYPE[3,3]:="''STREAM_TYPE'"
$ OLD_FILE="''NAME'"+"''TMP_TYPE'"+"_LOCK"+"_"+"''STREAM_NAME_OLD'"
$ NEW_FILE="''NEW_NAME'"+"''NEW_TYPE'"+-
  "_"+"''STREAM_NAME'_''FILTER_NAME'_''TIME_STAMP'"
$ STA_TYPE=F$PARSE(NEW_FILE,,,"TYPE")
$ STA_TYPE[3,3]:="STA"
$ STA_FILE="''NEW_NAME'"+"''STA_TYPE'"
$ RCP_FILE="''NEW_FILE'"RCP
$ WR "OLD_FILE: ''OLD_FILE' NEW FILE: ''NEW_FILE'"
$ WR "INP FILE: ''INP_FILE' RCP FILE: ''RCP_FILE'"
$!------------------------------------------------
$!Get the raw stream name
$!------------------------------------------------
$ OFL_STREAM="''STREAM_NAME'"
$!------------------------------------------------
$! If this is to be a null file, get rid of it and 
$! proceed to the next stream
$!------------------------------------------------
$   EVENT_LIST_FILE="''tape_name'.LIST_''F$EXTRACT(0,3,Stream_name_old)'"
$!
$ IF STREAM_OUT.EQS."NULL" 
$   THEN
$   DELETE/NOCONFIRM/noLOG PM$DATA:'OLD_FILE';*
$   if F$search(EVENT_LIST_FILE).nes."" then DELETE/NOCONFIRM/noLOG PM$RUN:'EVENT_LIST_FILE';*
$   WR " data file end list file has been deleted "
$   GOTO STREAM_LOOP
$   ENDIF
$!------------------------------------------------
$! Put the output files in the correct location as follows:
$!
$!    STREAM_OUT .eqs. "TAPE"     OUTPUT_DESTINATION="" 
$!                                MED_TYPE="5"     
$!    STREAM_OUT .eqs. "BUFFER"   OUTPUT_DESTINATION="D0$DATA$DST:"
$!                                MED_TYPE="1"     
$!    STREAM_OUT .eqs. "NULL"     PM$NULL     !null device
$!    STREAM_OUT .eqs. "TEST"     OUTPUT_DESTINATION="D0$DATA$DST:"
$!                                MED_TYPE="1"
$!
$!------------------------------------------------
$  OUTPUT_DESTINATION="D0$DATA$DST:"
$  MED_TYPE="1"                         !DISK FILE
$!
$ IF STREAM_OUT.EQS."TAPE" !TAPE FILES
$  THEN 
$  OUTPUT_DESTINATION=""
$  MED_TYPE="5"             !DOUBLE DENSITY TAPE
$  ENDIF
$!------------------------------------------------
$!   STREAM TYPE DEPENDANT THINGS
$!------------------------------------------------
$ IF STA_STREAMS THEN  DATATYPE=200   ! STA
$ IF DST_STREAMS THEN  DATATYPE=300   ! DST
$!
$!------------------------------------------------
$!   get file size
$!------------------------------------------------
$ file_size = F$FILE_ATTRIBUTES("PM$DATA:''OLD_FILE'","EOF")
$ MBYTES=(F$INTEGER('FILE_SIZE')*100+204799)/204800 !SIZE IN MBYTES
$!------------------------------------------------
$! ADDITIONAL SUMMARY FILE INFORMATION
$!------------------------------------------------
$  NSE=F$INTEGER(STREAM_EVENTS)
$  SUMMARY_LINE = F$FAO("!8SL!2AS!8SL",NQID," o",NSE)+" ''NEW_FILE'"
$  WRITE SUMMARY "+''SUMMARY_LINE'" 
$!------------------------------------------------
$! The generic name has a "P" when the filename has a "_"
$! between run and partition.
$!------------------------------------------------
$ GEN_MERGE_TYPE=MERGE_TYPE
$ IF MERGE_TYPE .EQS. "_" THEN GEN_MERGE_TYPE="P"
$ GEN_NAME="//FNAL/D0/CLDA/92P1E18/''STREAM_TYPE'01"+-
 "''RECO'''VREC'/''RAW_STREAM'''STREAM_NAME'''FILTER_NAME'"+-
 "/''RUN_DIRE'/R''RUN_NUMBER'''GEN_MERGE_TYPE'''PARTITION'"
$WR " GENERIC NAME: ''GEN_NAME'"
$!------------------------------------------------
$!This is the type SPECIFIC stuff for rcp file
$!------------------------------------------------
$ DEFINE/nolog RCPOUT PM$RUN:'RCP_FILE'
$!
$PM_MAKE_RCP/INRCP=RCPFILE/OUTRCP=RCPOUT/BANK_NAME=FILE_INFO_RCP-
  /OFFLINE_STREAM_NAME='OFL_STREAM'-
  /OUTPUT_FILENAME='OUTPUT_DESTINATION''NEW_FILE'-
  /MEDIUM_TYPE=%'MED_TYPE'/FILE_SIZE=%'MBYTES'/FILE_FORMAT=FX-
  /EVENTS=%'STREAM_EVENTS'
$!
$PM_MAKE_RCP/INRCP=RCPOUT/BANK_NAME=FILE_INFO_RCP-
/USER_WORDS=-
(%'DATATYPE',%0,%'FIRST_RUN',%'first_event',%'LAST_RUN',%'last_event',%'STREAM_EVENTS',%0,%0,%0) -
/GENERIC_NAME=('GEN_NAME')                 !MUST BE IN () TO PRESERVE THE /'S
$PM_MAKE_RCP/INRCP=RCPOUT/BANK_NAME=FILE_INFO_RCP-
/INPUT_TABLE_NAME='PDB_IN_TABLE'-
/OUTPUT_TABLE_NAME='PDB_OUT_TABLE'
$!------------------------------------------------
$!   PURGE the RCP file
$!------------------------------------------------
$ PURGE/noLOG/nocon RCPOUT
$!------------------------------------------------
$!  SUBMIT THE QCD_COMPRESS JOB
$!------------------------------------------------
$IF (F$EXTRACT(0,3,STREAM_NAME) .EQS. "QC2") .AND. (DST_STREAMS)
$      then
$      qmd_list = F$parse(NEW_FILE,,,"name")+".qcd_compress"
$      open/write ql pm$run:'qmd_list'
$      write ql new_file
$      close ql
$      IF STREAM_OUT .EQS. "TEST" ! Test job
$         THEN
$           copy/nolog PM$DATA:'OLD_FILE' PM$TEST:'NEW_FILE'
$!          The following test job will fail, because the file is not IN the
$!          in the input area. This tests the machinary.
$           if $status .eq. 1 then jm_sub qcd_compr 'qmd_list' 'qmd_queue'
$           rename/nolog pm$run:'qmd_list' pm$test:'qmd_list'
$         ELSE
$           copy/nolog PM$DATA:'OLD_FILE' PM$qmd:'NEW_FILE'
$           if $status .eq. 1 then jm_sub qcd_compr 'qmd_list' 'qmd_queue'
$           rename/nolog pm$run:'qmd_list' PM$qmd:'qmd_list'
$         ENDIF
$      endif
$!------------------------------------------------
$! Put the output files in the correct location as follows:
$!
$!    STREAM_OUT .eqs. "TAPE"     PM$TAPE     !tape area
$!    STREAM_OUT .eqs. "BUFFER"   PM$BUFFER   !buffer area
$!    STREAM_OUT .eqs. "NULL"     PM$NULL     !null device
$!    STREAM_OUT .eqs. "TEST"     PM$TEST     !test area
$!
$!------------------------------------------------
$!------------------------------------------------
$!  EVERYTHING HAS COMPLETED SUCCESSFULLY
$!  Rename the output files from *.*_LOCK to PM$'STREAM_OUT':*.* 
$!  PUT THE RCP FILES there also
$!------------------------------------------------
$    RENAME/noLOG PM$DATA:'OLD_FILE' PM$'STREAM_OUT':'NEW_FILE'
$    IF $STATUS 
$    THEN
$      rename/nolog PM$RUN:'RCP_FILE'  PM$'STREAM_OUT':*
$      WR " data file and RCP file has been renamed "
$!
$      if F$search(EVENT_LIST_FILE).nes.""
$        then
$!------------------------     Here the file name is added to the event list file
$        IF "''F$EXTRACT(0,3,FILTER_NAME)'" .EQS. "NON" 
$        THEN 
$          DELETE/NOCONFIRM/nolog 'EVENT_LIST_FILE';*
$          WR " list file has been deleted "
$        ELSE
$          OPEN/WRITE LIST_FILE 'EVENT_LIST_FILE'
$          WRITE LIST_FILE "!------------------------------------------------------------"
$          WRITE LIST_FILE "! ''NEW_FILE' "
$          WRITE LIST_FILE "!------------------------------------------------------------"
$          CLOSE LIST_FILE
$          APPEND/nolog 'EVENT_LIST_FILE';-1 'EVENT_LIST_FILE'
$!------------------ The following trick reformats the file correctly
$          EDT 'EVENT_LIST_FILE'
EXIT
$          PURGE/nolog/nocon 'EVENT_LIST_FILE'
$          RENAME/nolog 'EVENT_LIST_FILE'  - 
     PM$SUMMARY:'filname'.LIST_'F$EXTRACT(0,3,FILTER_NAME)'_'PROJect_code'
$          WR " list file has been renamed "
$        ENDIF
$      endif
$    ELSE
$      FILENFG="''RCP_FILE'"-"RCP"+"NFG"
$      COPY/noLOG PM$RUN:'RCP_FILE' PM_WORKSPACE:'FILENFG'
$      DELETE/noLOG/NOCONF PM$RUN:'RCP_FILE';*
$      WR " ************* ERROR coping data file *********************"
$    ENDIF
$!
$ GOTO STREAM_LOOP
$!
$STREAM_END:
$!----------
$  WR "  "
$  OPEN/READ INPUTS 'INPDIR''INPLIST'
$LOOP_FILES1_del:
$  READ/END_OF_FILE=EOF1_del INPUTS LINE
$  MARKFILE="''f$parse(LINE,,,"NAME")'''f$parse(LINE,,,"TYPE")'"
$!------------------------------------------------
$! DELETION WILL HAVE TO BE TURNED ON FOR PROCESSING
$! TAPE DSTS.
$!------------------------------------------------
$  if F$search("proman:[input]''MARKFILE'").nes. ""
$    then
$    WR "DELETE INPUT FILE ''markfile'"
$    DELETE/NOCONFIRM/log  proman:[input]'MARKFILE';*
$    endif
$!
$GOTO LOOP_FILES1_del
$!
$EOF1_del:
$  CLOSE INPUTS
$!
$ delete/noconfirm/nolog 'INPDIR''INPLIST';*
$ERROR:
$!------------------------------------------------
$! CLOSE THE SUMMARY FILE
$!------------------------------------------------
$ CLOSE SUMMARY
$ END_TIME=F$TIME()
$EXIT:
$ del/nocon/nolog PM$run:*'jobname'*.*;*
$ del/nocon/nolog 'tape_name'.*;*
$ del/nocon/nolog pm$run:for%%%.dat;*
$   time_now = F$TIME()
$   WR "  "
$   WR "STREAM_SUCCESS: Job ended at ''time_now'"
$   WR "  "
$! COMPUTE TIMES FOR EACH PHASE OF PROCESSING
$CALL DELTA_TIME "''START_TIME'" "''START_RECO_TIME'"
$STARTUP_TIME=DELTA_TIME
$CALL DELTA_TIME "''START_RECO_TIME'" "''END_RECO_TIME'"
$RECO_TIME=DELTA_TIME
$CALL DELTA_TIME "''END_RECO_TIME'" "''END_TIME'"
$ACCOUNTING_TIME=DELTA_TIME
$NODE=F$TRNLNM("SYS$NODE")-"::"
$WR "Time in seconds: Startup ''STARTUP_TIME'",-
    " Reco ''RECO_TIME' Accounting ''ACCOUNTING_TIME' Node ''NODE'"
$   EXIT
$!========================================================================
$! Name      : DELTA_TIME
$!
$! Purpose   : Compute DELTA TIME in seconds
$!
$!             $CALL DELTA_TIME "''START_TIME'" "''END_TIME'"
$!             $TIME=DELTA_TIME
$! Arguments : p1 initial clock time (from time=f$time())
$!
$! Created  25-FEB-1993   Lee Lueking
$!
$!========================================================================
$DELTA_TIME:    SUBROUTINE
$! clock time
$beg=f$cvtime("''p1'")
$end=f$cvtime("''P2'")
$months=f$integer(f$extract( 5,2,end))-f$integer(f$extract( 5,2,beg))
$days  =f$integer(f$extract( 8,2,end))-f$integer(f$extract( 8,2,beg))
$hours =f$integer(f$extract(11,2,end))-f$integer(f$extract(11,2,beg))
$mins  =f$integer(f$extract(14,2,end))-f$integer(f$extract(14,2,beg))
$secs  =f$integer(f$extract(17,2,end))-f$integer(f$extract(17,2,beg))
$DELTA_TIME==secs+60*mins+60*60*hours+60*60*24*days
$ENDSUBROUTINE
$secs  =f$integer(f$extract(17,2,end))-f$integer(f$extract(17,2,beg))
$DELTA_TIME==secs+60*mins+60*60*hours+60*60*24*days
$ENDSUBROUTINE

