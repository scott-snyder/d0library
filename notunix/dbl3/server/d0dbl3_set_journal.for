      SUBROUTINE D0DBL3_SET_JOURNAL(IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To setup FZ file for journaling
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: IOK   If .false. then trouble
C-
C-   Created  25-FEB-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL IOK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      CHARACTER*120 JNAME
      CHARACTER*10 TOPNM
      INTEGER IOS, ERR, TRULEN
      CHARACTER*2 CHOP
      CHARACTER*10 COPT
      INTEGER NUM,CLEN
      LOGICAL IER
C
      IOK = .TRUE.
C
      IF(OPTJ) THEN
        CALL INTMSG( ' Already one journal file open. Please close.')
        GOTO 999
      ELSEIF(.NOT. DBINI) THEN
        CALL INTMSG( ' Database is not initialized. Procedure aborted.')
        GOTO 999
      ENDIF
C
      CALL D0DBL3_JOFFNAM(JNAME,IER)
      IF(.NOT. IER) THEN
        CALL INTMSG
     &  (' D0DBL3_SET_JOURNAL: Error in file name for journal file.')
        IOK = .FALSE.
        GOTO 999
      ENDIF
C
      IF(CHOPJ(1:1) .EQ. 'X') THEN
        CHOP = 'XO'
      ELSEIF(CHOPJ(1:1) .EQ. 'A') THEN
        CHOP = 'AO'
      ELSE
        CHOP = 'O'
      ENDIF
C
      CLEN = TRULEN(JNAME)
C
      CALL GTUNIT(171,JUNIT,ERR)
C
      IF(CHOP(1:2) .EQ. 'AO') THEN
        OPEN (UNIT=JUNIT,FILE=JNAME(1:CLEN),ACCESS='SEQUENTIAL',
     +      FORM='FORMATTED',RECORDTYPE='FIXED',CARRIAGECONTROL='LIST',
     +      RECL=80,STATUS='NEW',IOSTAT=IOS)
      ELSE
        OPEN(UNIT=JUNIT, STATUS='NEW', FILE=JNAME(1:CLEN),
     &      FORM='UNFORMATTED',IOSTAT=IOS)
      ENDIF
      IF(IOS .NE. 0) THEN
        IQUEST(1)= IOS
        CALL ERRDB('ERROR OPENING FILE')
        GOTO 999
      ENDIF
C
      CALL FZFILE(JUNIT,0,CHOP)
C
      TOPNM = TOPN(1)
      CALL DBFZOP(JUNIT, TOPNM, ' ')
      IF(IQUEST(1) .NE. 0) THEN
        CALL ERRDB('ERROR OPENING JOURNAL FILE')
        GOTO 998
      ENDIF
C
      OPTJ = .TRUE.
C
      GOTO 999
C----------------------------------------------------------------------
  998 CONTINUE
      CLOSE(JUNIT)
      CALL RLUNIT(171,JUNIT,ERR)
C
  999 RETURN
      END
