      SUBROUTINE DBCLB_SET_JOURNAL(TOPNAME,INT,FILNAM,CHOPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To setup FZ file for journaling
C-
C-   Inputs  : TOPNAME    ; Top directory name
C-                INT     ; If .true. then interactive. In this case the rest
C-                           of arguments are dummies.
C-                FILNAM  ; name of journal file
C-                CHOPT   ; ' ', 'X', 'A' For local, exchange, and alpha mode
C-   Outputs :
C-   Controls:
C-
C-   Created  25-FEB-1991   SHAHRIAR ABACHI
C-   Modified 10-SEP-1991   SHAHRIAR ABACHI  Exchange format included
C-   Modified 01-JUN-1992   SHAHRIAR ABACHI  Non interactive features added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOPNAME
      LOGICAL INT
      CHARACTER*(*) FILNAM,CHOPT
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      CHARACTER*120 JNAME
      INTEGER IOS, ERR
      CHARACTER*2 CHOP
      CHARACTER*10 COPT
      CHARACTER*14 FRMT(3)
      INTEGER NUM,CLEN
      DATA FRMT /'LOCAL','EXCHANGE-X','EXCHANGE-A'/
C
      IF(OPTJ) THEN
        CALL INTMSG( ' Already one journal file open. Please close.')
        GOTO 999
      ENDIF
C
      IF(.NOT. CALL_DBEND) THEN
        CALL INTMSG( ' Database is not initialized. Procedure aborted.')
        GOTO 999
      ENDIF
C
      IF(INT) THEN
        CALL GETPAR(1, ' Enter journal file name >  ', 'C', JNAME)
        IF(JNAME .EQ. ' ') THEN
          CALL INTMSG(' No file name selected. Procedure aborted')
          GOTO 999
        ENDIF
C
        CALL GETOPT(1,' Select FZ format ',3,FRMT,NUM)
        COPT = FRMT(NUM)
        CHOP = 'O'
        IF(COPT(10:10) .EQ. 'X') CHOP = 'XO'
        IF(COPT(10:10) .EQ. 'A') CHOP = 'AO'
      ELSE
        JNAME = FILNAM
        IF(CHOPT(1:1) .EQ. 'X') THEN
          CHOP = 'XO'
        ELSEIF(CHOPT(1:1) .EQ. 'A') THEN
          CHOP = 'AO'
        ELSE
          CHOP = 'O'
        ENDIF
      ENDIF
      CALL STR$TRIM(JNAME,JNAME,CLEN)
C
      CALL GTUNIT(15,JUNIT,ERR)
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
      CALL DBFZOP(JUNIT, TOPNAME, ' ')
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
      CALL RLUNIT(15,JUNIT,ERR)
C
  999 RETURN
      END
