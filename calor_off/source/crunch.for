      SUBROUTINE CRUNCH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-     Read in runs and events and call user event processing routine.
C-     This routine contains five user hooks, which are built by the
C-     program builder.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Hooks   :
C-
C-      CAL_BEGIN_RUN
C-              Called AFTER a begin-of-run record has been read in.
C-
C-      CAL_EVENT
C-              Called AFTER an event record has been read in.
C-
C-      CAL_END_RUN
C-              Called AFTER an end-of-run record has been read in
C-
C-   Created  23-JAN-1989   Harrison B. Prosper, John Womersley
C-   Updated  21-SEP-1989   Chip Stewart,  HBP
C-   Updated  21-SEP-1989   Chip Stewart
C-   Updated  10-OCT-1989   Harrison B. Prosper
C-   Added CAL_EVENT_DUMP, CAL_EVENT_RESET
C-   Updated  24-OCT-1989   Chip Stewart   - Put run division wipe in EVTIN
C-   Updated  26-JAN-1990   Harrison B. Prosper
C-      Added Skip/Read control words to CINJOB
C-   Updated  26-JUL-1990   Harrison B. Prosper
C-      Added Trigger selection function
C-   Updated   1-AUG-1990   Harrison B. Prosper
C-      Make frame write out begin-run and end-run records
C-   Updated  13-SEP-1990   Harrison B. Prosper
C-      Added path selection
C-   Updated  19-SEP-1990   K. Wyatt Merritt
C-      Changed handling of event dump requests and output streams
C-   Updated  18-DEC-1990   Harrison B. Prosper
C-      CAL_END_RUN called when EOR is TRUE
C-   Updated  30-JAN-1991 Chip Stewart
C-      Decouple EVENT_CONTROL and TRIGGER_BIT_CHECK
C-   Updated   5-NOV-1991   Krzysztof L. Genser
C       to handle FATMEN long generic names
C-   Updated  12-DEC-1991   Harrison B. Prosper
C-      Add END_PROCESSING switch
C-   Updated   3-FEB-1992   Chip Stewart (HBP too)
C-      Redefined BOR to be true if run number changes -
C-   Updated  19-FEB-1992   Chip Stewart
C-       IF NEVMAX negative and (NEVENT.GE.-NEVMAX) then goto next run
C-   Updated   8-APR-1992   M. Diesburg
C-      Removed CAL_EVENT_DUMP and CAL_EVENT_RESET declarations for IBM
C-      compatibility.
C-   Updated   1-MAY-1992   Harrison B. Prosper
C-      Check if PIXIE package is ON
C-   Updated  19-MAY-1992   Harrison B. Prosper
C-      Use EVOPIN
C-   Updated  22-MAY-1992   Krzysztof L. Genser
C-      Handle format of the file specified using FATMEN generic names
C-   Updated   9-JUN-1992   Harrison B. Prosper
C-      Correct counting of runs
C-   Updated   8-Dec-1992   Alexander n. Peryshkin
C-      Release unit number ( Add RLUNIT)
C-   Updated  12-DEC-1992   Norman A. Graf
C-      Added KEEP_BRR and KEEP_EOR to drop begin and end run records
C-   Updated  15-DEC-1992   Harrison B. Prosper
C-      Send summary to SSUNIT as well as to the screen
C-   Updated  16-DEC-1992   Stan M. Krzywdzinski
C-      Call ERRSUM
C-   Updated  22-JAN-1993   Harrison B. Prosper
C-   Updated   4-MAY-1993   Rajendran Raja   ADDED WEIGHT LOGIC
C-   Updated  14-APR-1994   Rajendran Raja   Added NTUPLE logic
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FATPARA.DEF/LIST'
      INCLUDE 'D0$INC:FILE_WT.INC'
      INTEGER LENOCC
      INTEGER IRC
      CHARACTER*4 CFLF
C
      LOGICAL CAL_BEGIN_RUN                ! Get parameters for begining of run
      LOGICAL CAL_EVENT                    ! Event processing
      LOGICAL CAL_END_RUN                  ! End-of-run etc. processing
C
      LOGICAL ERROR,DONE,EVENT,BOR,EOR,EOF,EOD,OK,QUIT
      LOGICAL TRIGGER_BIT_CHECK,END_PROCESSING,NEW_RUN
C
      INTEGER PRINT_EVENT_NUMBER
      INTEGER INUNIT,NFILE,IOS,ERR,I,J,K,L,M,N,IER
      INTEGER NEVENT,NEVENT1,NEVMAX,NWARN,NEVDMP
      INTEGER NUMBER_RECORDS, NUMBER_BOR,NUMBER_EOR,NUMBER_RUNS
      INTEGER NUMBER_EVENTS_NOTOK, NUMBER_EVENTS_OK
      INTEGER NMOD,LAST_RUN,CURRENT_RUN,RUNNO
      INTEGER STATUS
C
      INTEGER MXCONT
      PARAMETER( MXCONT = 50 )
      INTEGER CONTROL(MXCONT),NCONT
      LOGICAL DATAFILE,PROCESS,STAND_OUT,DST_OUT,DMPUSR,WRITE_STREAMS
      LOGICAL FLGVAL,DUMP_THIS_EVENT, WRITE_THIS_EVENT
      LOGICAL PIXIE, EVENT_DISPLAY
      LOGICAL WRITE_BOR,WRITE_EOR
      LOGICAL READ_NTUPLE
C
      CHARACTER*4  PATH, XOPT
      CHARACTER*80 COMMAND,STRING
      CHARACTER*255 FILNAM,FILENM
      CHARACTER*80 REMARK
      CHARACTER D0DADFILE*200,CURRENT_FILE*200,LAST_FILE*200
C
      INTEGER LEN_FIL,TRULEN,SSUNIT,LOUT(2)
      REAL    WT
      INTEGER IEVFILE, LF
      SAVE D0DADFILE,CURRENT_FILE,LAST_FILE
C
C----------------------------------------------------------------------
C
C ****  Book end processing switch
C
      CALL FLGBK ('END_PROCESSING',1)
      CALL FLGBK ('READ_NTUPLE',1)
      CALL FLGSET('READ_NTUPLE',.FALSE.)
C
      IFILE = 0
      LAST_RUN = 0
      CALL EZPICK ('CALFRAME_RCP')
      CALL EZGET_i  ('PRINT_EVENT_NUMBER',PRINT_EVENT_NUMBER,IER)
      CALL EZGET_i  ('NUMBER_OF_WARNINGS',NWARN,IER)
      CALL EZGET_i  ('NUMB_EVENT_DUMPS',NEVDMP,IER)
      CALL EZGETS ('PATH',1,PATH,L,IER)  ! needs to preceed PATHGT for IER test
      IF ( IER .NE. 0 ) THEN
        CALL PATHGT(PATH)
      ENDIF
      CALL PATHDF(PATH)           ! Set default path
      CALL PATHRS
C
C ****  Set EVENT_DISPLAY flag
C
      CALL PBD_GET_FLAG('PIXIE',PIXIE)
      IF ( PIXIE ) THEN
        CALL EZGET('EVENT_DISPLAY',EVENT_DISPLAY,IER)
        CALL FLGSET('EVENT_DISPLAY',EVENT_DISPLAY)
      ENDIF
C
      CALL EZGET  ('WRITE_BOR',WRITE_BOR,IER)
      CALL EZGET  ('WRITE_EOR',WRITE_EOR,IER)
      CALL EZRSET
C
      WRITE_STREAMS = FLGVAL('WRITE_STREAMS')
C
      IF ( NWARN .EQ. 0 ) NWARN = 1
      CALL ERRMAX (' ',-1,NWARN)
      IF ( PRINT_EVENT_NUMBER .GT. 0 ) THEN
        CALL ERRSON
      ELSE
        CALL ERRSOF
      ENDIF
C
      NUMBER_RECORDS          = 0
      NUMBER_EVENTS_NOTOK     = 0
      NUMBER_EVENTS_OK        = 0
      NUMBER_BOR              = 0
      NUMBER_EOR              = 0
      NUMBER_RUNS             = 0
C
C *****************************************
C ****  START OF LOOP OVER DATA FILES  ****
C *****************************************
      NFILE = 0                         ! Zero file counter
    5 CONTINUE
C
C ****  Get data file name, number of events to process and
C       read in the appropriate calibration file. COMMAND and CONTROL(i)
C       controls the event skipping/processing. NEVENT is zeroed by
C       CINJOB.
C
      CALL CINJOB(FILNAM,NEVMAX,QUIT,COMMAND,CONTROL,NCONT,NEVENT,
     &  XOPT,WT)
      IF ( QUIT ) GOTO 999
C
C ****  Translate file-name
C
      LEN_FIL = TRULEN(FILNAM)
      CALL TRNLNM(FILNAM(1:LEN_FIL),FILENM,L)
      LEN_FIL = L
C
      CALL EZPICK('CALFRAME_RCP')
      CALL EZ_STORE_NAME('INPUT_FILENAME',FILNAM(1:LEN_FIL),IER)
      CALL EZRSET
C
      IF ( FILNAM(1:6) .EQ. 'NOFILE' ) THEN
        DATAFILE = .FALSE.
      ELSE
        DATAFILE = .TRUE.
        REMARK = 'PROCESSING NEW Datafile '//FILENM(1:LEN_FIL)
        CALL ERRMSG('NEW_FILE','CRUNCH',REMARK,'S')
        IF ( IFILE.LT.MX_FILE ) THEN
          IFILE = IFILE + 1
          FILE_NAMES(IFILE) = FILENM(1:LEN_FIL)
          WEIGHTS(IFILE) = WT
        ENDIF
      ENDIF
C
C ****  Open Data File and return input unit number
C
      NFILE = NFILE + 1
      IEVFILE = 0
      IF ( DATAFILE ) THEN
C
C ****  distinguish between 'normal' and generic names
C
        IF (FILENM(1:11).EQ.'READ_NTUPLE'  ) THEN
C
          READ_NTUPLE = .TRUE.
          CALL FLGSET('READ_NTUPLE',.TRUE.)
          CALL EZPICK('CALFRAME_RCP')
          CALL DO_HBOOK_OPEN('NTUPLE_FILES',STATUS)
          IF ( STATUS.NE.0 ) THEN
            CALL ERRMSG('CALOR_OFF','CRUNCH',
     &        'CANNOT OPEN NTUPLE FILE ','F')
            OK=.FALSE.
          ELSE
            OK = .TRUE.
          ENDIF
          CALL EZRSET
        ELSEIF ( FILENM(1:9) .NE. D0TRNK  ) THEN
C
          CALL EVOPIN(FILENM(1:L),XOPT,INUNIT,OK)
C
        ELSE
C
C ****  generic name
C
C
C ****  use D0OPEN to open the file via FATMEN
C
          CALL RLUNIT(87,INUNIT,ERR)
          CALL GTUNIT(87,INUNIT,ERR)
          IF(ERR.NE.0) GOTO 100
C
C ****  options for FMOPEN after / (the slash)  Read,
C ****  V update file size after staging to disk, F call FZFILE
C
          CALL D0OPEN(INUNIT,FILENM,'/RVF',OK)
          IF(.NOT.OK) GOTO 100
C
C ****  get the file format
C
          CALL FMGETC (LFMINF,CFLF,MFLFFA,NFLFFA,IRC)
          IF ( IRC.NE.0 ) THEN
            OK = .FALSE.
            GOTO 100
          ENDIF
          XOPT=' '
          IF ( CFLF(1:LENOCC(CFLF)) .EQ. 'FX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FFX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FXN' ) XOPT='X'
        ENDIF
C
  100   CONTINUE
        IF ( .NOT. OK ) THEN
          REMARK = ' Unable to OPEN DataFile '//FILENM(1:LEN_FIL)
          CALL ERRMSG('BAD_OPEN','CRUNCH',REMARK,'W')
          GOTO 999
        ENDIF
      ENDIF
C
C ****************************
C ****  EVENT LOOP BEGINS ****
C ****************************
C
   10 CONTINUE
C
C ****  READ DATA FROM ZEBRA FILE
C
      IOS = 0
      IF ( READ_NTUPLE ) THEN
        NEVENT1 = NEVENT + 1
        CALL CAL_READ_NTUPLE(NEVENT1,IOS)
      ELSE
        IF ( DATAFILE ) THEN
          CALL EVTIN (INUNIT,IOS)
        ENDIF
      ENDIF

C
C ****  Update ZEBRA error code in CAL control file
C
      CALL ZEBIOS ('CALFRAME_RCP',IOS,-1)
C
      EVENT = IOS .EQ. 0                ! Set Event flag
      BOR   = IOS .EQ. 1                ! Set Begin-of-Run flag
      EOR   = IOS .EQ. 2                ! Set End-of-Run flag
      EOF   = IOS .EQ. 3                ! Set End-of-File flag
      EOD   = IOS .GE. 4                ! Set End-of-Data flag
      DONE  = .FALSE.                   ! Clear done processing flag
C
      IF ( BOR .OR. EOR. OR. EVENT ) THEN
        NUMBER_RECORDS  = NUMBER_RECORDS + 1
        LAST_RUN        = CURRENT_RUN
        CURRENT_RUN     = RUNNO()
        NEW_RUN         = CURRENT_RUN.NE.LAST_RUN
C
C ****  CHECK D0DAD FILENAME
C
        IF(INDEX(FILENM,'D0DAD').GT.0) THEN
          CALL D0DAD_FZFILENAME(D0DADFILE)
          CALL SWORDS(D0DADFILE,L,M,N)
          IF(M.GT.0) THEN
            LAST_FILE     = CURRENT_FILE
            CURRENT_FILE  = D0DADFILE
            IF(.NOT.NEW_RUN) NEW_RUN = CURRENT_FILE.NE.LAST_FILE
          END IF
        ELSE
C
C ****  CHECK OTHER FILENAMES FOR NEW RUN
C
          LAST_FILE     = CURRENT_FILE
          CURRENT_FILE  = FILENM
          IF(.NOT.NEW_RUN) NEW_RUN = CURRENT_FILE.NE.LAST_FILE
        ENDIF
      ENDIF
C
      IF ( BOR. OR. NEW_RUN ) THEN      ! BEGIN-OF-RUN
C
C ****  BEGIN-OF-RUN-RECORD or NEW RUN
C
        IF ( NEW_RUN ) THEN
          NUMBER_RUNS = NUMBER_RUNS + 1
        ENDIF
C
        OK = CAL_BEGIN_RUN()            ! Do user begin of run processing
C
        IF ( BOR ) THEN
          NUMBER_BOR = NUMBER_BOR + 1
          IF (WRITE_STREAMS.AND. WRITE_BOR) CALL EVTWOS        ! Write out begin-run record
        ENDIF
C
      END IF
C
      IF ( EVENT ) THEN
C
C ****  EVENT-RECORD
C
        IEVFILE = IEVFILE + 1
        NEVENT = NEVENT + 1             ! Increment event counter
        DONE   = (NEVMAX.GT.0) .AND. (NEVENT.GE.NEVMAX)
        DONE   = DONE .OR. ((NEVMAX.LT.0).AND.(NEVENT.GE.-NEVMAX))
C
C ****  Check whether or not to skip or process this event
C
        CALL EVENT_CONTROL (NEVENT,COMMAND,CONTROL,NCONT,PROCESS)
C
C ****  Process event if trigger bits OK
C
        IF ( PROCESS .AND. TRIGGER_BIT_CHECK ('CALFRAME_RCP') ) THEN
C
C ****  Set path and build the top level banks beneath that path.
C ****  Also set up a few of the links in /ZLINKC/.
C
          IF ( DATAFILE ) CALL CZLINK
C
          OK = CAL_EVENT()              ! Do user event processing

          END_PROCESSING = FLGVAL('END_PROCESSING')
          DONE = DONE .OR. END_PROCESSING
C
C ***   Check if a single event dump has been requested
C
          DUMP_THIS_EVENT = FLGVAL('DUMP_THIS_EVENT') .OR.
     &      NEVENT.LE.NEVDMP
C
          IF ( DUMP_THIS_EVENT ) THEN
            CALL CAL_EVENT_DUMP()  ! DUMP requested banks
            CALL FLGSET('DUMP_THIS_EVENT',.FALSE.)
          ENDIF
C
          CALL CAL_EVENT_RESET()        ! Things to be done BEFORE
C                                       ! writing out event
          IF ( OK ) THEN
            NUMBER_EVENTS_OK = NUMBER_EVENTS_OK + 1
C
            IF ( PRINT_EVENT_NUMBER.GT.0 ) THEN
              NMOD = MOD(NEVENT,PRINT_EVENT_NUMBER)
              IF(NMOD.EQ.0.OR.NEVENT.EQ.1)THEN
                WRITE(REMARK,'(I10)') NEVENT
                REMARK = ' *** Processed event *** '//REMARK(1:10)
                CALL ERRMSG('SUCCESS','CRUNCH',REMARK,'S')
              ENDIF
            ENDIF
C
C ***   Check if event writing to output streams is enabled
C
            CALL WRITE_LOGIC(WRITE_THIS_EVENT)
C
            IF (WRITE_THIS_EVENT) THEN
              CALL EVTWOS   ! Write to output stream
            ENDIF
C
          ELSE
            NUMBER_EVENTS_NOTOK = NUMBER_EVENTS_NOTOK + 1
C
C ****  Something wrong!
C
            IF ( PRINT_EVENT_NUMBER.GT.0 ) THEN
              NMOD = MOD(NEVENT,PRINT_EVENT_NUMBER)
              IF(NMOD.EQ.0.OR.NEVENT.EQ.1)THEN
                WRITE(REMARK,'(I10)') NEVENT
                REMARK = ' *** Skipped event processing *** '//
     &            REMARK(1:10)
                CALL ERRMSG('SKIPPED','CRUNCH',REMARK,'S')
              ENDIF
            ENDIF
          ENDIF
C
        ENDIF
C
      ENDIF
      IF ( EOR ) THEN
C
C ****  END-OF-RUN-RECORD
C
        OK = CAL_END_RUN()                      ! Do user end-of-run processing
        NUMBER_EOR = NUMBER_EOR + 1
        IF ( WRITE_STREAMS .AND. WRITE_EOR ) CALL EVTWOS        ! Write out end-of-run record
      ENDIF
C
C ********************************
C ****  Check "end" flags     ****
C ********************************
C
      IF ( DONE .OR. EOR .OR. EOF .OR. EOD ) THEN
        WRITE(6,FMT=
     &    '('' CRUNCH: NUMBER OF EVENTS READ '',I8,'' '')') NEVENT
      ENDIF
      IF ( EOF ) THEN
        EVENTS(IFILE) = IEVFILE
        IEVFILE = 0
      ENDIF
C
C ********************************
C ****  Terminate, close and
C ****  release data file
C ********************************
C
      IF ( DONE .OR. EOF .OR. EOD ) THEN
C
C ****  CHECK for -NEVMAX events per FILE in DATFILE_LIST
C
        IF ((NEVMAX.LT.0).AND.(NEVENT.GE.-NEVMAX)) THEN
          OK = CAL_END_RUN()            ! Do user end-of-run processing
          NUMBER_EOR = NUMBER_EOR + 1
        END IF
        IF(FILNAM(1:6).NE.'NOFILE') CALL CZEND (INUNIT)
        GOTO 5                          ! Get next data file
      ELSE
        GOTO 10                         ! Get next event
      ENDIF

  999 CONTINUE
C
C ****  Simple summary
C
      LOUT(1) = 6
      LOUT(2) = SSUNIT()
C
      DO I = 1, 2
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'')')
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   NUMBER OF RUNS PROCESSED          '',I8,'' '')')
     &    NUMBER_RUNS
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   NUMBER OF BEGIN RUN RECORDS READ  '',I8,'' '')')
     &    NUMBER_BOR
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   NUMBER OF END RUN RECORDS READ    '',I8,'' '')')
     &    NUMBER_EOR
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   NUMBER OF EVENTS PROCESSED OK     '',I8,'' '')')
     &    NUMBER_EVENTS_OK
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   NUMBER OF EVENTS PROCESSED NOT OK '',I8,'' '')')
     &    NUMBER_EVENTS_NOTOK
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   ----------------------------------'',8(''-''))')
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   NUMBER OF RECORDS READ            '',I8,'' '')')
     &    NUMBER_RECORDS
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'')')
C
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'')')

        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   Following files were processed'' )')
        WRITE(LOUT(I),FMT=
     &    '('' CRUNCH   Filename, Number of events, Weight'' )')
        DO 1 J = 1 , IFILE
          LF = TRULEN(FILE_NAMES(J))
          LF = MIN(LF,50)
          WRITE(LOUT(I),FMT=
     &      '('' CRUNCH: '',A50,1X,I8,F12.3)')
     &      FILE_NAMES(J)(1:LF),EVENTS(J),WEIGHTS(J)
    1   CONTINUE
      ENDDO
C
C ****  Give a summary of errors
C
      CALL ERRSUM(LOUT(2))
C
      RETURN
      END
