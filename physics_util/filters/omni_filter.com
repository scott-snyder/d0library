$!========================================================================
$!
$! Name      : OMNI_FILTER
$!
$! Purpose   : Procedure to run the omni_filter job
$!
$! Arguments : 
$!
$! Created  14-JAN-1993   Lee Lueking
$! Modified 10-MAR-1993   Lee Lueking Supports both DST and STA streaming with 
$!                        latest OMNI_FILTER
$! Modified 21-SEP-1993   Dorota I. Genser - changes for NEW JOBMAN
$!
$!========================================================================
$   WR "  "
$   WR "filter: Job started at ''F$time()'"
$   WR "  "
$!
$!
$  DSTDIR="NL:"
$  STADIR="NL:"
$!
$ IF STA_STREAMS then STADIR="''F$TRNLNM("PM$DATA")'"
$ IF DST_STREAMS then DSTDIR="''F$TRNLNM("PM$DATA")'"
$!
$!================================== checks pm$run disk ==========================================================
$ CHECK_DISK_run:
$ blocks_free = f$getdvi("PM$run","FREEBLOCKS")
$ IF BLOCKS_FREE.LT.50000
$ THEN
$   WR "Insufficient free blocks ''BLOCKS_FREE' at ''f$time()'  - disk ''F$TRNLNM("pm$run")'"
$   WAIT 0:2:00
$   GOTO CHECK_DISK_run
$ ENDIF
$!------------------------------------------------
$!
$!  Create the input RCP file to RUN D0RECO
$!
$!------------------------------------------------
$!
$RCPFILE="pm$run:''INPLIST'_RCP"
$pm_MAKE_RCP/OUTRCP='RCPFILE'/BANK_NAME=FILES_RCP/INPUT_AREA='INPDIR'
$NFILES=1
$OPEN/READ INPUTS 'INPDIR''INPLIST'
$LOOP_INPUT:
$READ/END_OF_FILE=EOF INPUTS LINE
$ line = F$edit(line,"trim,compress,upcase")
$ line = F$parse(line,,,"name")+ F$parse(line,,,"type")
$IF NFILES .EQ. 1 
$ THEN 
$   INPFILE=="''LINE'"
$   pm_MAKE_RCP/INRCP='RCPFILE'/BANK_NAME=FILES_RCP/INPUT_FILES='LINE'
$ ELSE
$   pm_MAKE_RCP/INRCP='RCPFILE'/BANK_NAME=FILES_RCP/ADD/INPUT_FILES='LINE'
$ ENDIF
$ NFILES=NFILES+1
$ GOTO LOOP_INPUT
$ EOF:
$ CLOSE INPUTS
$!
$! Data and RCP file symbols
$!
$ STAFILE == F$PARSE(INPFILE,,,"NAME")+F$EXTRACT(0,15,F$PARSE(INPFILE,,,"TYPE"))
$ DSTTYPE == F$EXTRACT(0,15,F$PARSE(INPFILE,,,"TYPE"))
$ LEN=F$LENGTH(DSTTYPE)
$ DSTTYPE == "''f$extract(0,3,dsttype)'DST''f$extract(6,len,dsttype)'"
$ STATYPE == "''f$extract(0,3,dsttype)'STA''f$extract(6,len,dsttype)'"
$ FILNAME == F$PARSE(INPFILE,,,"NAME")
$ DSTFILE == F$PARSE(INPFILE,,,"NAME")+"''DSTTYPE'"
$ DST_FILES=="''DSTDIR'''DSTFILE'"
$ STAFILE == F$PARSE(INPFILE,,,"NAME")+"''STATYPE'"
$ STA_FILES=="''STADIR'''STAFILE'"
$!
$ tape_name == "pm$run:''project_name'_''FILNAME'"
$ pm_MAKE_RCP/INRCP='RCPFILE'/BANK_NAME=FILES_RCP/TAPE_NAME='tape_name'
$ pm_MAKE_RCP/INRCP='RCPFILE'/BANK_NAME=FILES_RCP/DST_AREA='DSTDIR'/DST_FILES=('DSTFILE'_LOCK) -
     /STA_AREA='STADIR'/STA_FILES=('STAFILE'_LOCK)
$ purge/nocon/nolog 'rcpfile'
$   wr "  "
$   wr "============================================================"
$   wr "======== Input Directory : ''INPDIR'"
$   wr "======== INPUT FILE      : ''INPFILE'"
$   wr "======== STA Directory   : ''STADIR'"
$   wr "======== DST Directory   : ''DSTDIR'"
$   wr "========     STA file    : ''STAFILE'"
$   wr "========     DST FILE    : ''DSTFILE'"
$   wr "======== FILES RCP FILE  : ''RCPFILE'"
$   wr "======== NODE            : ''NODE'"
$   wr "============================================================"
$   wr "  "
$!------------------------------------------------
$!
$!  Setup and  RUN D0RECO
$!
$!------------------------------------------------
$@prod$omni_filter:OMNI_FILTER_d0reco junk junk junk junk
$@prod$omni_filter:OMNI_FILTER_versions
$ RECO_VERS="''F$INTEGER(f$extract(11,4,dsttype))'"
$ WR "Setup for project ''PROJECT_NAME' reco version ''RECO_VERS'"
$@prod$omni_filter:OMNI_FILTER_SETUP "''PROJECT_NAME'" "''RECO_VERS'"
$!
$! We use THE FILES_RCP FOR FILE INFORMATION
$!   
$   DEFINE/nolog FILES_RCP "''RCPFILE'"
$!
$!------------------------------------------------
$!
$!  Parse the file name for the run period
$!
$!------------------------------------------------
$!
$ LEN=F$LENGTH(F$PARSE(inpfile,,,"NAME"))
$ RUN_NUMBER=F$INTEGER(F$EXTRACT(LEN-9,6,inpfile))
$ IF (RUN_NUMBER.LT.45161)
$ THEN
$   WR "THIS IS BEFORE ANY KNOWN RUN STREAMS_FILTER PERIOD"
$   $STATUS == 4
$   GOTO EXIT
$ ENDIF
$!------------------------------------------------
$! DEFINITION for fake_e_candidate ntuple
$! (Send to null device for DST's)
$!------------------------------------------------
$ DEFINE/nolog FAKE_E_CANDIDATE_NTUPLE NLA0:
$ if FAKE_E_CANDIDATE_NTUPLE then FAKE_E_CANDIDATE_NTUPLE PM$RUN:'FILNAME'.FAK_NT
$!------------------------------------------------
$!   RUN D0RECO
$!------------------------------------------------
$ CHECK_DISK:
$ blocks_free = f$getdvi("PM$DATA","FREEBLOCKS")
$ IF BLOCKS_FREE.LT.MIN_BLOCKS 
$ THEN
$   WR "Insufficient free blocks ''BLOCKS_FREE' at ''f$time()'"
$   WAIT 0:05:00
$   GOTO CHECK_DISK
$ ENDIF
$ START_RECO_TIME==F$TIME()
$   set rms/extend=1200
$   WR "filter: Start D0RECO at ''F$TIME()'"
$   WR "  "
$   D0RECO
$   WR "filter: End   D0RECO at ''F$TIME()'"
$   WR "  "
$ END_RECO_TIME==F$TIME()
$   set rms/extend=50
$   del/nocon/nolog 'rcpfile';*
$   EXIT
