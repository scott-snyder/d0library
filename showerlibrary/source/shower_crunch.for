      SUBROUTINE SHOWER_CRUNCH
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
C-      CAL_EVENT_DUMP
C-              Called AFTER an event has been processed.
C-
C-      CAL_EVENT_RESET
C-              Called before event is written out.
C-
C-      CAL_END_RUN
C-              Called AFTER an end-of-run record has been read in, or
C-              at end-of-data or end-of-file.
C-
C-   Created  23-JAN-1989   Harrison B. Prosper, John Womersley
C-   Updated  21-SEP-1989   Chip Stewart,  HBP
C-   Updated  21-SEP-1989   Chip Stewart
C-   Updated  10-OCT-1989   Harrison B. Prosper
C-   Added CAL_EVENT_DUMP, CAL_EVENT_RESET
C-   Updated  24-OCT-1989   Chip Stewart   - Put run division wipe in EVTIN
C-   Updated  26-JAN-1990   Harrison B. Prosper
C-      Added Skip/Read control words to CINJOB
C-   Updated   1-JUL-1992   W.G.D.Dharmaratna, made it compatible with new
C-                                            CINJOB with XOPT
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FATPARA.DEF/LIST'
   
C
      LOGICAL CAL_BEGIN_RUN                ! Get parameters for begining of run
      LOGICAL CAL_EVENT                    ! Event processing
      LOGICAL CAL_EVENT_DUMP               ! Dump events
      LOGICAL CAL_EVENT_RESET              ! Called before writing event.
      LOGICAL CAL_END_RUN                  ! End-of-run etc. processing
C
      LOGICAL ERROR,DONE,EVENT,BOR,EOR,EOF,EOD,OK,QUIT
      INTEGER PRINT_EVENT_NUMBER
      INTEGER INUNIT,NFILE,IOS,ERR,I,J,K,L,M,N,IER
      INTEGER NEVENT,NEVMAX,NWARN,NEVDMP
      INTEGER NMOD
C
      INTEGER MXCONT
      PARAMETER( MXCONT = 50 )
      INTEGER CONTROL(MXCONT),NCONT
      LOGICAL DATAFILE,PROCESS
C
C ****
C
      CHARACTER*80 COMMAND
      CHARACTER FILNAM*80,FILENM*132
      CHARACTER EVTMSG*80,REMARK*80
      CHARACTER*4 XOPT,CFLF
C
      INTEGER LENOCC,IRC
      INTEGER LEN_FIL,TRULEN
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL NO_BOR
      DATA NO_BOR/.TRUE./               ! NO BEGIN RUN RECORDS YET
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK ('CALFRAME_RCP')
        CALL EZGET  ('PRINT_EVENT_NUMBER',PRINT_EVENT_NUMBER,IER)
        CALL EZGET  ('NUMBER_OF_WARNINGS',NWARN,IER)
        CALL EZGET  ('NUMB_EVENT_DUMPS',NEVDMP,IER)
        CALL EZRSET
        IF ( NWARN .EQ. 0 ) NWARN = 1
        CALL ERRMAX ('CALOR_OFF',-1,NWARN)
        CALL ERRMAX ('CALORIMETER',-1,NWARN)
      ENDIF
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
      CALL CINJOB(FILNAM,NEVMAX,QUIT,COMMAND,CONTROL,NCONT,NEVENT,XOPT)
      IF ( QUIT ) GOTO 999
C
      LEN_FIL = TRULEN(FILNAM)
      CALL EZPICK('CALFRAME_RCP')
      CALL EZ_STORE_NAME('INPUT_FILENAME',FILNAM(1:LEN_FIL),IER)
      CALL EZRSET
C
      IF(FILNAM(1:6).EQ.'NOFILE')THEN
        DATAFILE = .FALSE.
      ELSE
        DATAFILE = .TRUE.
        CALL ERRMSG('CALORIMETER','CRUNCH',
     &    'PROCESSING NEW Datafile '//FILNAM,'W')
      ENDIF
C
C ****  Declare input file to ZEBRA (basically calls FZFILE)
C       and return input unit number
C
      NFILE = NFILE + 1
      LEN_FIL = TRULEN(FILNAM)
      CALL TRNLNM(FILNAM(1:LEN_FIL),FILENM,L)
      LEN_FIL = L
c      IF ( DATAFILE ) THEN
c        CALL EVOPIN(FILENM(1:L),XOPT,INUNIT,OK)
C      IF(DATAFILE)CALL CZOPEN (FILNAM,INUNIT)
c      END IF
      IF ( DATAFILE ) THEN
C
C ****  distinguish between 'normal' and generic names
C
        IF ( FILENM(1:9) .NE. D0TRNK  ) THEN
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
      IF(DATAFILE)CALL EVTIN (INUNIT,IOS)
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
      IF (NO_BOR .OR. BOR ) THEN   ! BEGIN-OF-RUN
        NO_BOR = .FALSE.                ! IN CASE FIRST FILE HAS NO
                                        ! BEGIN RUN RECORD
        OK = CAL_BEGIN_RUN()            ! Do user begin of run processing
      ENDIF
C
      IF ( EVENT ) THEN
C
        NEVENT = NEVENT + 1           ! Increment event counter
        DONE = (NEVMAX.GT.0) .AND. (NEVENT.GE.NEVMAX)
C
C ****  Check whether or not to process this event
C
        CALL EVENT_CONTROL (NEVENT,COMMAND,CONTROL,NCONT,PROCESS)
C
        IF ( PROCESS ) THEN
C
          IF(DATAFILE)CALL CZLINK       ! Build RECO etc.(only if HEAD exists)
C
          OK = CAL_EVENT()              ! Do user event processing
C
          IF(NEVENT.LE.NEVDMP .AND.
     &     NEVDMP.GT.0)     THEN        ! Check whether to dump event
            CALL FLSETS('DMPUSR',.TRUE.)
          ENDIF
C
          CALL CAL_EVENT_DUMP()         ! DUMP requested banks
C
          CALL CAL_EVENT_RESET()        ! Things to be done BEFORE
C                                       ! writing out event
          IF ( OK ) THEN
C
            IF ( PRINT_EVENT_NUMBER.GT.0 ) THEN
              NMOD = MOD(NEVENT,PRINT_EVENT_NUMBER)
              IF(NMOD.EQ.0.OR.NEVENT.EQ.1)THEN
                CALL ERRMSG('CALOR_OFF','CRUNCH',
     &          '*** Processed this event ***','W')
              ENDIF
            ENDIF
C
            CALL EVTWOS                 ! Write to output stream
          ENDIF
C
        ENDIF
      ENDIF
C
C ********************************
C ****  Check remaining flags ****
C ********************************
C
      IF(.NOT.DATAFILE)DONE = .TRUE.    ! ONE SHOT
      IF ( (DONE .OR. EOR .OR. EOF .OR. EOD) .AND.NEVENT.NE.0) THEN
C
C ****  ONLY DO END OF RUN PROCESSING IF SOME EVENTS WERE PROCESSED.
C
        WRITE(6,FMT='('' NUMBER OF EVENTS READ '',I8,'' '')') NEVENT
C
        OK = CAL_END_RUN()              ! Do user end-of-run processing
        NEVENT = 0                      ! NOW GET THE NEXT FILE
        NO_BOR = .TRUE.                 ! ALSO NEXT FILE MAY NOT HAVE BOR
C
C ****  THIS HAS THE EFFECT OF SETTING NEVMX TO REFER TO A SINGLE
C ****  FILE THAN THE WHOLE FILE_LIST
C
      ENDIF
C
      IF ( DONE .OR. EOF .OR. EOD ) THEN
C
C ****  Terminate, close and release data file
C
        IF(FILNAM(1:6).NE.'NOFILE')CALL CZEND (INUNIT)
        GOTO 5                          ! Get next data file
      ELSE
        GOTO 10                         ! Get next event
      ENDIF
C
  999 RETURN
      END
