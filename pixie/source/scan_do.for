      SUBROUTINE SCAN_DO
C----------------------------------------------------------------------
C-   Purpose and Methods : System called Routine for Modifying Scan Sheet
C-   Controls: pf4 for saving into the bank (BACK)
C-
C-   Created  14-DEC-1992   Vipin Bhatnagar
C-   Modified 15-DEC-1992   Nobuaki Oshima
C-        Added new ENTRY SCAN_SAVE_INFO for the SCAN_SAVE.
C-   Modified 13-JAN-1993   Vipin Bhatnagar
C-   Modified 28-JAN-1993   Lupe Howell  Error check after EZPICK, moved
C-        EZRSET fo first block
C-   Modified 11-MAY-1993   Vipin Bhatnagar (No Answer->No Scan Bank)
C-   Updated  23-MAY-1993   Vipin Bhatnagar
C-     No.of questions = 55, Max. no. of scan banks=9
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL FIRST
      LOGICAL EZERROR
      DATA FIRST /.TRUE./
      INTEGER NQUES,ALLOWQ,LENGTH,IDS,I,K,IER
      PARAMETER ( ALLOWQ=55 )
      CHARACTER*10 QUES(ALLOWQ),KQUES(ALLOWQ)
      CHARACTER*30 SDATE,SBTIME,SETIME
      CHARACTER*76 MESG
      CHARACTER LOPT
C
      CHARACTER*40 PROMPT(ALLOWQ),KPROMPT(ALLOWQ),REM(ALLOWQ)
      CHARACTER*20 OUTSTR(ALLOWQ),KOUTSTR(ALLOWQ)
      INTEGER NPROM,KPROM,LENSTR(ALLOWQ)
      INTEGER GZPROC,LPROC,IZLINK,LADDR,STATUS
      INTEGER NZBANK,NLIBNK,ISTOR
      INTEGER RUNNO,EVONUM
      INTEGER RUNSAV,EVTSAV,NRUN,NEVT
      LOGICAL MODFLG(ALLOWQ)
      LOGICAL SCAN_INI
      EXTERNAL SCAN_INI
      SAVE    RUNSAV,EVTSAV
      DATA    RUNSAV,EVTSAV /-999, -999/
C
C----------------------------------------------------------------------
C
C Get RCP file
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        IF ( SCAN_INI() ) THEN
          CALL EZPICK('PX_SCANNING_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('PIXIE','SCAN_DO',
     &        'Can NOT find PX_SCANNING_RCP','W')
            GO TO 999
          ENDIF
          CALL EZGETS('SCAN_DATE',1,SDATE,LENGTH,IER)
          CALL EZGETS('SCAN_BTIME',1,SBTIME,LENGTH,IER)
          CALL EZGETS('SCAN_ETIME',1,SETIME,LENGTH,IER)
          CALL EZ_GET_CHARS('QUESTION_LIST',NQUES,QUES,IER)
          NPROM=NQUES
C
C Fill questions into prompt array (IDS=1 for question)
C
          IDS=1
          DO I=1,NQUES
            CALL EZGETS(QUES(I),IDS,PROMPT(I),LENGTH,IER)
          END DO
          CALL EZRSET
        ENDIF
      ENDIF

C----------------------------------------------------------------------
C
      NRUN = RUNNO()
      NEVT = EVONUM()
      IF (NRUN.NE.RUNSAV .OR. NEVT.NE.EVTSAV) THEN
        RUNSAV = NRUN
        EVTSAV = NEVT
C-
C----Checking the existance of 9 SCAN banks
C-
        LPROC = GZPROC()
        IF(LPROC.EQ.0) THEN
          CALL ERRMSG('SCANNING','SCAN_START', 'PROC BANK NOT SET UP',
     &      'W')
          GOTO 999
        ENDIF
        ISTOR  = 1
        IF(IQ(LPROC-2) .LE. 7) THEN
          LADDR=0
        ELSE
          IZLINK = 8
          LADDR  = LQ(LPROC-IZLINK)
        ENDIF
        NLIBNK = NZBANK(ISTOR,LADDR)
        IF (NLIBNK.GE.9) THEN
          MESG=
     &      'Nine scan banks already exist, either quit or rescan'
          CALL OUTMSG(MESG)
          CALL GETPAR(1,'...so [Q]uiting or [R]escanning?','C',LOPT)
          IF (LOPT(1:1).EQ.'Q'.OR.LOPT(1:1).EQ.'q') THEN
            CALL FLGSET('ACTIVE_SCAN',.FALSE.)
            GOTO 999
          ELSEIF (LOPT(1:1).EQ.'R'.OR.LOPT(1:1).EQ.'r') THEN
            GOTO 555
          ENDIF
        ENDIF
  555   CONTINUE
        CALL SCAN_START(NPROM,PROMPT,OUTSTR)
      ENDIF
C-
C---Prompting the Scanner for the Answers
C-
      CALL DIAPOS(3,2)
      CALL DIABOX('SCAN QUESTIONS',NPROM,PROMPT,OUTSTR,LENSTR,' ')
      DO I=1,NPROM
        IF(LENSTR(I).GT.0) THEN
          CALL INTMSG(' '//PROMPT(I)//': '//OUTSTR(I)(1:LENSTR(I)))
        END IF
      END DO
C-
C---Checking some answer is given or not(to save in scan bank)
C-
      IF(OUTSTR(1).EQ.'        ') THEN
        CALL FLGSET('ACTIVE_SCAN',.FALSE.)
      END IF
C-
      GOTO 999
C-
C--- Give Saving/Replacing answers to SCAN_SAVE
C-
      ENTRY SCAN_SAVE_INFO(KQUES,KPROM,KPROMPT,KOUTSTR)
C-
      DO K=1,NPROM
        KQUES(K)   = QUES(K)
        KPROMPT(K) = PROMPT(K)
        KOUTSTR(K) = OUTSTR(K)
      ENDDO
      KPROM = NPROM
C-
  999 RETURN
      END
