      SUBROUTINE DBCLB_UPDATE_NEW(CHOPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To update from FZ backup or journal file
C-
C-   Inputs  :
C-                CHOPT  ;  Charcter option ('O' from external source)
C-   Outputs :
C-   Controls:
C-
C-   Created  25-FEB-1991   SHAHRIAR ABACHI
C-   Modified 10-SEP-1991   SHAHRIAR ABACHI  Exchange format included
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHOPT
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      CHARACTER*40 FILNM
      INTEGER LUNIT, ERR, LOGL, IOS
      CHARACTER*2 CHOP
      CHARACTER*10 COPT
      CHARACTER*14 FRMT(3)
      INTEGER NUM
      DATA FRMT /'LOCAL','EXCHANGE-X','EXCHANGE-A'/
C
      IF(.NOT. CALL_DBEND) THEN
        CALL INTMSG( ' Database is not initialized. Procedure aborted.')
        GOTO 999
      ENDIF
C
      CALL GETPAR(1, ' Enter input file name > ', 'C', FILNM)
      IF(FILNM .EQ. ' ') THEN
        CALL INTMSG(' No file selected. Update aborted.')
        GOTO 999
      ENDIF
C
      CALL GTUNIT(15,LUNIT,ERR)
C
      CALL GETOPT(1,' Select FZ format ',3,FRMT,NUM)
      COPT = FRMT(NUM)
      CHOP = 'I'
      IF(COPT(10:10) .EQ. 'X') CHOP = 'XI'
      IF(COPT(10:10) .EQ. 'A') CHOP = 'AI'
C
      IF(CHOP(1:2) .EQ. 'AI') THEN
        OPEN (UNIT=LUNIT,FILE=FILNM,ACCESS='SEQUENTIAL',
     +      FORM='FORMATTED',RECORDTYPE='FIXED',CARRIAGECONTROL='LIST',
     +      RECL=80,STATUS='OLD',IOSTAT=IOS)
      ELSE
        OPEN(UNIT=LUNIT, STATUS='OLD', FILE=FILNM,
     &      FORM='UNFORMATTED',IOSTAT=IOS)
      ENDIF
C
      IF(IOS .NE. 0) THEN
        IQUEST(1)= IOS
        CALL ERRDB('ERROR OPENING FILE')
        GOTO 998
      ENDIF
C
      LOGL = -1
      CALL FZLOGL(LUNIT,LOGL)
C
      CALL FZFILE(LUNIT,0,CHOP)
C
      CALL DBFZUP(LUNIT, CHOPT)
      IF(IQUEST(1) .NE. 0) THEN
        CALL ERRDB('ERROR UPDATING FILE')
      ENDIF
C
      CALL FZENDI(LUNIT,'T')
C
  998 CONTINUE
      CLOSE(LUNIT)
      CALL RLUNIT(15,LUNIT,ERR)
C
C----------------------------------------------------------------------
  999 RETURN
      END
