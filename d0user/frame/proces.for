      SUBROUTINE PROCES(INUNIT,STATE,NOIN)
C-------------------------------------------------------------------------------
C-
C-  Read in runs and events and call analyzing subroutine
C-  handle interrupt menu
C-
C-   INPUT:
C-   INUNIT= ZEBRA input file unit number
C-   NOIN  = .TRUE. if no ZEBRA input file was requested
C-
C-   ENTRY JOBSUM : start job summary
C-
C-     SDP July,1987
C-         Modified November,1988
C-   Updated  20-AUG-1991   Susan K. Blessing  Add call to ANALYZE_EVT(OK).
C-    USREVT will only be called if OK=.TRUE. Changed definition of NSKIP.
C-    NSKIP events will be skipped from the beginning of the file, not from
C-    the beginning of each run in the file.  NSKIP can be used to skip events
C-    from multiple files in Manual Process.  Changed D0USER_xxxxx.OUT output
C-    to give more exact information on number of skipped, processed and read
C-    events.
C-    Cleaned up a little.
C-   Updated  10-OCT-1991   Susan K. Blessing   Correct the calculation of
C-    the number of events for runs with more than one part.
C-   Updated   3-APR-1992   Krzysztof L. Genser  FATMEN Interface
C-    longer file name buffers 76 --> 255
C-   Updated  29-JUL-1992   Krzysztof L. Genser  
C-    use FZENDI and RLUNIT ater CLOSE
C-   Updated  20-AUG-1992   Serban Protopopescu  
C-      add ENTRY INPUT_FILE_NAME to return input file name
C-   Updated  16-AUG-1993   Susan K. Blessing  Add RETURN statement to 
C-    ENTRY JOBSUM.
C-
C-------------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FATPARA.DEF/LIST'
C
      CHARACTER*(*) F_NAME
      INTEGER LENOCC
      EXTERNAL DISUSR
      INTEGER INUNIT,L,TRULEN,I,NWSTA,NWDST
      INTEGER IOS,NRUNB,NRUNC,OUT,ERR,SSUNIT,N
      INTEGER NRUN,NEVT,NEVRUN,NSKIP,NTOT,NPROC,EVTCNT,NCNT
      INTEGER RUNNO,USNVRN
      INTEGER RUNS(1000),EVRUNS(1000),EVSKIP(1000),EVPROC(1000),ISTORH
      INTEGER EVREAD(1000)
      INTEGER NFILE,FEVSKIP(1000),FEVPROC(1000),FEVREAD(1000)
C
      CHARACTER*8 STATE
      CHARACTER*12 COMAND
      CHARACTER*32 TITLE
      CHARACTER*255 INPUT_FILE
      CHARACTER*255 FILE(1000)
      CHARACTER*255 TMPFNAME
      CHARACTER*78 MSGSTA
      CHARACTER*78 MSG
      CHARACTER*16 STNDRD
      CHARACTER*4 CFLF
      CHARACTER*2 XOPT
C
      LOGICAL INTERR,PAUSCH,NOMORE,INTREQ,STATUS
      LOGICAL NOIN,FIN,ENDR,GOTOP,GOON,OK,GOOD
      LOGICAL YES,FLGVAL,BLOOP,USREVT,DAQ,USRPAR
C
      DATA NFILE/0/
      DATA NRUNC/0/
      DATA NRUN/-1/
      DATA NRUNB/-1/
      DATA NTOT/0/
      DATA NPROC/0/
      DATA EVSKIP/1000*0/
      DATA EVPROC/1000*0/
      DATA EVREAD/1000*0/
      DATA FEVSKIP/1000*0/
      DATA FEVPROC/1000*0/
      DATA FEVREAD/1000*0/
C-------------------------------------------------------------------------------
C
      FIN=.FALSE.
      NEVT=0
C
C        Reset flags
C
      PAUSCH=.FALSE.
      STATUS=.FALSE.
      BLOOP=.FALSE.
      DAQ=FLGVAL('READ_FROM_DAQ')
      CALL UFLSET
      CALL USRZEV            ! zero event arrays if necessary
C
C           dialog for storing histograms
C
      ISTORH=0
      CALL GETPAR(1,'Store histograms ? 0=NO, 1=after each run, 2='
     &    //'end of job only >','I',ISTORH)
      IF(ISTORH.NE.1) CALL SETSTR(.FALSE.)
      IF(ISTORH.EQ.1) CALL SETSTR(.TRUE.)
      IF(ISTORH.NE.2) CALL FLGSET('STORE_HISTOS',.FALSE.)
      IF(ISTORH.EQ.2) CALL FLGSET('STORE_HISTOS',.TRUE.)
C
C         check for input file
C
      IF(.NOT.FLGVAL('NO_INPUT_FILE')) THEN
        IF(NOIN) CALL ZBINPF(INUNIT,NOIN)
      ENDIF
      CALL INPUT_F_INFO(INPUT_FILE,XOPT)
      IF ( INPUT_FILE(1:4).NE.'NONE' ) THEN
        WRITE(MSGSTA,101) INPUT_FILE(1:64)
        CALL STAMSG(MSGSTA,.TRUE.)
        IF(.NOT.GOON()) THEN
          CALL FLGSET('MORE_EVENTS',.TRUE.)
          CALL DIAL_EVENTS(NSKIP)
          NCNT = NTOT
          NFILE = NFILE + 1
          FILE(NFILE) = INPUT_FILE
          FEVSKIP(NFILE) = NSKIP
          IF (NFILE.EQ.2) THEN
            FEVREAD(1) = NTOT
          ELSE IF (NFILE.GT.2) THEN
            FEVREAD(NFILE-1) = NTOT
            DO I = 1, NFILE-2
              FEVREAD(NFILE-1) = FEVREAD(NFILE-1) - FEVREAD(I)
            END DO
          END IF
          CALL FLGSET('MORE_EVENTS',.FALSE.)
        ENDIF
      ELSE
        IF(.NOT.GOON()) THEN
          CALL FLGSET('MORE_EVENTS',.TRUE.)
          CALL DIAL_EVENTS(NSKIP)
          NCNT = NTOT
          CALL FLGSET('MORE_EVENTS',.FALSE.)
        ENDIF
        CALL STAMSG(' Data is being generated',.TRUE.)
      ENDIF
C
C          event loop
C
   10 CONTINUE
C
C          reset counters if more than 1000 files
        IF(NFILE.GT.999.OR.NRUNC.GT.999) 
     &     CALL RUNS_SUMMARY(NTOT,NRUNC,RUNS,EVRUNS,EVSKIP,EVPROC,
     &     EVREAD,NFILE,FILE,FEVSKIP,FEVPROC,FEVREAD,OUT)
C
      NTOT=EVTCNT()
      NEVRUN=USNVRN()
      IF(NOMORE()) FIN=.TRUE.
      IF((NEVRUN.GT.0.AND.NTOT.GE.NEVRUN).OR.FIN) THEN
        CALL ENDRUN
        GOTO 40     ! processed as many events as requested
      ENDIF
C
      IOS=0
C
      IF ( INTREQ() ) THEN       ! check for INTERRUPT menu request
        BLOOP=.TRUE.
        IF (FLGVAL('PAUSE')) THEN
          PAUSCH=.TRUE.
        ELSE
          PAUSCH=.FALSE.
          IF(DAQ) CALL GET_DAQ_EVENT(IOS)
          IF(.NOT.NOIN) CALL EVTRD(INUNIT,IOS)  ! read events from file
        ENDIF
C
      ELSE
        PAUSCH=.FALSE.
        IF(DAQ) CALL GET_DAQ_EVENT(IOS)
        IF(.NOT.NOIN) CALL EVTRD(INUNIT,IOS)  ! read events from file
      ENDIF
C
C           event record
      IF(IOS.EQ.0) THEN
        IF(NRUN.NE.RUNNO()) GOOD=USRPAR()       ! user run parameters
        NRUN=RUNNO()
        IF(.NOT.GOOD) GOTO 10   ! skip run
C
        IF(NRUNB.NE.RUNNO()) THEN  ! There was no b-of-r record
          NEVT=0
          CALL IF_CANMEN
          CALL INIRUN(NSKIP)
C
          NEVRUN=USNVRN()
          CALL STNRUN(.TRUE.)
          FIN=.FALSE.
          ENDR=.FALSE.
          NRUNC=NRUNC+1
          IF (NRUNC.EQ.2) THEN
            EVREAD(1) = NTOT
          ELSE IF (NRUNC.GT.2) THEN
            EVREAD(NRUNC-1) = NTOT
            DO I = 1, NRUNC-2
              EVREAD(NRUNC-1) = EVREAD(NRUNC-1) - EVREAD(I)
            END DO
          END IF
          NRUNB=NRUN
          RUNS(NRUNC)=NRUN
        ENDIF
C
C
        IF(INTERR().AND.NEVT.GT.1) THEN  ! run with interrupt menu
          IF(FLGVAL('PAUSE')) THEN
            PAUSCH=.TRUE.
          ELSE
            PAUSCH=.FALSE.
          ENDIF
          IF(.NOT.FLGVAL('EXAMINE')) THEN
            TITLE=' D0USER INTERRUPT '
            CALL INTMEN(TITLE,'INTERRUPT',DISUSR)
          ENDIF
        ENDIF
C
        IF(.NOT.PAUSCH) THEN
          NEVT=NEVT+1
          IF(STATUS.NE.PAUSCH) THEN
            STATUS=PAUSCH
            CALL STAMSG(MSGSTA,.TRUE.)
          ENDIF
        ELSE
          CALL WAIBIT(1)         ! add a wait for paused state
          IF(STATUS.NE.PAUSCH) THEN
            STATUS=PAUSCH
            CALL STAMSG(' Data is NOT being processed',.TRUE.)
          ENDIF
        ENDIF
C
        IF((NTOT.LE.NEVRUN.OR.NEVRUN.LT.0).AND.
     &    (NTOT-NCNT.GE.NSKIP)) THEN
C
          OK = .TRUE.
          CALL ANALYZE_EVT(OK)
C
          IF(OK.AND..NOT.PAUSCH) THEN
            IF(.NOT.NOIN) CALL MKPATH   ! make headers (depending on path)
            OK=USREVT()                 ! event processing
            EVPROC(NRUNC) = EVPROC(NRUNC) + 1
            FEVPROC(NFILE) = FEVPROC(NFILE) + 1
          ENDIF
          IF(.NOT.OK) THEN
            CALL USRZEV            ! zero event arrays if necessary
            GOTO 10                ! failed, get next event
          ENDIF
C
          CALL EVENTS             ! handle commands for processed event
        ELSE IF ((NTOT-NCNT).LT.NSKIP) THEN
          EVSKIP(NRUNC) = EVSKIP(NRUNC) + 1
        ENDIF
C
        IF  (NTOT.LE.NEVRUN.OR.NEVRUN.LT.0) THEN
          EVRUNS(NRUNC) = NEVT
        END IF
C
C           Begin run record
      ELSE IF(IOS.EQ.1.AND.NRUN.NE.RUNNO()) THEN
        CALL IF_CANMEN
        IF(FLGVAL('EXAMINE')) CALL SETINQ(.TRUE.) ! needed for HISPAK
C
C        make sure end-of-run record can be added even if not on file
        IF(NRUNC.GT.0.AND..NOT.ENDR) CALL ENDRUN
        NEVT=0
        CALL INIRUN(NSKIP)
        NEVRUN=USNVRN()
        CALL STNRUN(.TRUE.)
        FIN=.FALSE.
        ENDR=.FALSE.
        NRUNC=NRUNC+1
        IF (NRUNC.EQ.2) THEN
          EVREAD(1) = NTOT
        ELSE IF (NRUNC.GT.2) THEN
          EVREAD(NRUNC-1) = NTOT
          DO I = 1, NRUNC-2
            EVREAD(NRUNC-1) = EVREAD(NRUNC-1) - EVREAD(I)
          END DO
        END IF
        NRUNB=RUNNO()
C
C           End run record
      ELSE IF(IOS.EQ.2) THEN ! call ENDRUN only for our end-of-run record
        IF(FLGVAL('EXAMINE')) CALL SETINQ(.TRUE.) ! needed for HISPAK
        IF(.NOT.ENDR) CALL ENDRUN
        ENDR=.TRUE.
        IF(NEVRUN.EQ.-1) GOTO 40  ! only one run was requested
C
C         end-of-file
      ELSE IF(IOS.GT.2) THEN

        IF(FLGVAL('EXAMINE')) CALL SETINQ(.TRUE.) ! needed for HISPAK
        CALL FZENDI(INUNIT,'T')
        CALL D0CLOSE(INUNIT, ' ', OK)
        CALL RLUNIT(87,INUNIT,ERR)
        CALL SEARCH_FILES(INPUT_FILE,ERR)
        IF(ERR.NE.0) GOTO 40             ! return for no more files
C                new file
C
C ****  check if this is not a FATMEN generic name
C
        TMPFNAME = INPUT_FILE
        CALL CLTOU(TMPFNAME)
C
        IF ( TMPFNAME(1:9) .NE. D0TRNK  ) THEN
C
          WRITE(MSGSTA,101) INPUT_FILE(1:64)
          CALL STAMSG(MSGSTA,.TRUE.)
          CALL EVOPIN(INPUT_FILE,XOPT,INUNIT,OK)  ! open input file
          IF(.NOT.OK) THEN
            CALL INTMSG(' Cannot open file:')
            L=TRULEN(INPUT_FILE)
            DO I=1,L,72
              MSG=' '//INPUT_FILE(I:I+72)
              CALL INTMSG(MSG)
            ENDDO
            GOTO 40
          ENDIF
C
        ELSE
C
C ****  this is a FATMEN generic name
C
          CALL GTUNIT(87,INUNIT,ERR)
          IF(ERR.NE.0) THEN
            CALL INTMSG(' Cannot get unit for file:')
            WRITE(MSG,103) INPUT_FILE
            CALL INTMSG(MSG)
            GOTO 40
          ENDIF
C
C ****  options for fmopen after '/' R read,  
C ****  V update file size after staging to disk, F call FZFILE
C
          CALL D0OPEN(INUNIT,INPUT_FILE,'/RVF',OK)
          IF(.NOT.OK) THEN
            CALL INTMSG(' Cannot open file:')
            WRITE(MSG,103) INPUT_FILE
            CALL INTMSG(MSG)
            GOTO 40
          ENDIF
C
C ****  get the file name corresponding to the generic name
C
          CALL FMGETC (LFMINF,INPUT_FILE,MFQNFA,255,ERR)
          IF(ERR.NE.0) THEN
            CALL INTMSG(' Cannot get DSN for file:')
            WRITE(MSG,103) INPUT_FILE
            CALL INTMSG(MSG)
            GOTO 40
          ENDIF
C
C ****  get the file format
C
          CALL FMGETC (LFMINF,CFLF,MFLFFA,NFLFFA,ERR)
          IF(ERR.NE.0) THEN
            CALL INTMSG(' Cannot get file format for file:')
            WRITE(MSG,103) INPUT_FILE
            CALL INTMSG(MSG)
            GOTO 40
          ENDIF
          XOPT=' '
          IF ( CFLF(1:LENOCC(CFLF)) .EQ. 'FX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FFX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FXN' ) XOPT='X'
C
          WRITE(MSGSTA,101) INPUT_FILE(1:64)
          CALL STAMSG(MSGSTA,.TRUE.)
C
        ENDIF
C
        NFILE = NFILE + 1
        FILE(NFILE) = INPUT_FILE
        FEVSKIP(NFILE) = -1
        IF (NFILE.EQ.2) THEN
          FEVREAD(1) = NTOT
        ELSE IF (NFILE.GT.2) THEN
          FEVREAD(NFILE-1) = NTOT
          DO I = 1, NFILE-2
            FEVREAD(NFILE-1) = FEVREAD(NFILE-1) - FEVREAD(I)
          END DO
        END IF
C
      ENDIF
C
C           Now handle INTERRUPT menu requests
C
      CALL UREQST
C
      IF(.NOT.PAUSCH.AND.BLOOP)
     &    CALL SETINQ(.FALSE.) ! done with INTERRUPT requests
C
      BLOOP=.FALSE.
      GOTO 10
C
C            Done with processing
C
   40 CONTINUE
      NTOT=EVTCNT()
      CALL EVTMSG(MSG(1:48))
      WRITE(MSGSTA,102) MSG(1:48),NTOT
      CALL STAMSG(MSGSTA,.TRUE.)
   50 CONTINUE
      STATE='FINISH'
      CALL FLGSET('EXAMINE',.FALSE.)
      CALL CANMEN
      IF(GOON().AND..NOT.NOMORE()) GOTO 999  ! end (automatic processing)
C
C                  manual processing
      CALL MENUDO('DONE_WITH_DATA','DONE_WITH_DATA',COMAND)
C
      IF(COMAND(1:4).EQ.'EXIT') GOTO 999
C
      IF(COMAND(1:6).EQ.'STATUS') THEN
        CALL EVTMSG(MSG(1:48))
        WRITE(MSG,102) MSG(1:48),NTOT
        CALL INTMSG(MSG)
        CALL USRPST
        GOTO 50
C
      ELSE IF(COMAND(1:4).EQ.'QUIT') THEN
        CALL QUIT
        GOTO 50
C
      ELSE IF(COMAND(1:7).EQ.'EXAMINE') THEN
        CALL HISPAK(.FALSE.)
        IF(GOTOP()) THEN
          STATE='SETUP'
          GOTO 999       ! return
        ELSE
          GOTO 50
        ENDIF
C
      ELSE IF(COMAND.EQ.'CONTINUE') THEN
        IF(FLGVAL('NO_INPUT_FILE')) THEN
          CALL GETPAR(1,' Change no. of events to process? Y/N>',
     &      'L',YES)
          IF(YES) CALL GETPAR(1,' Number of events>','I',NEVRUN)
          CALL STNVRN(NEVRUN)
        ELSE
          CALL GETPAR(1,' Want a new input file? Y/N >',
     $      'L',YES)
          IF(YES) THEN
            CALL ZBINPF(INUNIT,NOIN)
            CALL INPUT_F_INFO(INPUT_FILE,XOPT)
            WRITE(MSGSTA,101) INPUT_FILE(1:64)
            CALL STAMSG(MSGSTA,.TRUE.)
            CALL FLGSET('MORE_EVENTS',.TRUE.)
            CALL DIAL_EVENTS(NSKIP)
            NCNT = NTOT
            NFILE = NFILE + 1
            FILE(NFILE) = INPUT_FILE
            FEVSKIP(NFILE) = NSKIP
            IF (NFILE.EQ.2) THEN
              FEVREAD(1) = NTOT
            ELSE IF (NFILE.GT.2) THEN
              FEVREAD(NFILE-1) = NTOT
              DO I = 1, NFILE-2
                FEVREAD(NFILE-1) = FEVREAD(NFILE-1) - FEVREAD(I)
              END DO
            END IF
            CALL FLGSET('MORE_EVENTS',.FALSE.)
            NEVT=0
          ELSE
            CALL GETPAR(1,' Change no. of events to process? Y/N>',
     &        'L',YES)
            IF(YES) CALL GETPAR(1,' Number of events>','I',NEVRUN)
            CALL STNVRN(NEVRUN)
          ENDIF
          NEVRUN=USNVRN()
        ENDIF
        FIN=.FALSE.
        IF(NFILE.GT.999.OR.NRUNC.GT.999) 
     &     CALL RUNS_SUMMARY(NTOT,NRUNC,RUNS,EVRUNS,EVSKIP,EVPROC,
     &     EVREAD,NFILE,FILE,FEVSKIP,FEVPROC,FEVREAD,OUT)
        GOTO 10
C
      ELSE IF(COMAND(1:9).EQ.'SUMMARIES') THEN
        CALL SUMARY
        IF(GOTOP()) THEN
          STATE='SETUP'
          GOTO 999       ! return to setup
        ELSE
          GOTO 50
        ENDIF
C
      ELSE IF(COMAND(1:6).EQ.'FINISH') THEN
        STATE='FINISH'
        GOTO 999         ! return to finish
C
      ENDIF
C
      IF(GOTOP()) STATE='SETUP'   ! return to setup
  999 RETURN
C
C         Job summaries
C
      ENTRY JOBSUM
C
      CALL RUNS_SUMMARY(NTOT,NRUNC,RUNS,EVRUNS,EVSKIP,EVPROC,
     &  EVREAD,NFILE,FILE,FEVSKIP,FEVPROC,FEVREAD,OUT)
C
      CALL EVWRITES('STA',NWSTA)
      WRITE(OUT,1011) NWSTA
      IF(NWSTA.GT.0) CALL EVSUM_MULT(OUT,1)
      CALL EVWRITES('DST',NWDST)
      WRITE(OUT,1012) NWDST
      IF(NWDST.GT.0) CALL EVSUM_MULT(OUT,2)
      CALL EVCLWO('ALL')
      CALL ERRSUM(OUT)
C
      RETURN
C
      ENTRY INPUT_FILE_NAME(F_NAME)
      F_NAME=INPUT_FILE
      RETURN
C
  101 FORMAT(' Processing :',A64)
  102 FORMAT(A48,'  Total read =',I7)
  103 FORMAT(' ',A)
 1011 FORMAT( /,'  NUMBER OF RECORDS WRITTEN TO STA FILE',I7)
 1012 FORMAT( /,'  NUMBER OF RECORDS WRITTEN TO DST FILE',I7)
      END
