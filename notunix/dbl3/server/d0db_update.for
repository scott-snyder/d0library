      SUBROUTINE D0DB_UPDATE(PATH,FILNM,CHOP,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To update from FZ backup or journal file
C-
C-   Inputs  :
C-             PATH  :  Path name. NOT REQUIRED if journal file is used
C-             FILNM :  File name
C-             CHOP  :  Charcter option ('I, XI, AI'). If J is the first
C-                       character then it is a journal file.
C-   Outputs :
C-   Controls:    IOK   ;  If .false. then trouble
C-
C-   Created  25-SEP-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILNM, PATH
      CHARACTER*(*) CHOP
      LOGICAL JOPT,IOK
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER LUNIT, ERR, LOGL, IOS, IBANK, LBANK, CLEN
      CHARACTER*9 TEMP
      CHARACTER*4 BNKNAM
      CHARACTER*80 MSG
      EQUIVALENCE(CALIB_LNK(1),LBANK)
      EQUIVALENCE(IBANK,BNKNAM)
C
      IOK = .TRUE.
C
C      IF(.NOT. CALL_DBEND) THEN
C        CALL INTMSG( ' Database is not initialized. Procedure aborted.')
C        GOTO 999
C      ENDIF
C
      CALL GTUNIT(15,LUNIT,ERR)
C
      TEMP = CHOP
      IF(TEMP(1:1) .EQ. 'J') THEN
        JOPT = .TRUE.
        TEMP(1:) = TEMP(2:)
      ELSE
        JOPT = .FALSE.
      ENDIF
C
      IF(TEMP(1:2) .EQ. 'AI') THEN
C&IF VAXVMS
        OPEN (UNIT=LUNIT,FILE=FILNM,ACCESS='SEQUENTIAL',
     +      FORM='FORMATTED',RECORDTYPE='FIXED',CARRIAGECONTROL='LIST',
     +      RECL=80,STATUS='OLD',IOSTAT=IOS)
C&ELSE
C&        OPEN (UNIT=LUNIT,FILE=FILNM,ACCESS='SEQUENTIAL',
C&     +      FORM='FORMATTED',CARRIAGECONTROL='LIST',
C&     +      RECL=80,STATUS='OLD',IOSTAT=IOS)
C&ENDIF
      ELSE
        OPEN(UNIT=LUNIT, STATUS='OLD', FILE=FILNM,
     &      FORM='UNFORMATTED', READONLY, IOSTAT=IOS)
      ENDIF
C
      IF(IOS .NE. 0) THEN
        IQUEST(1)= IOS
        CALL ERRDB('ERROR OPENING FILE')
        IOK = .FALSE.
        GOTO 998
      ENDIF
C
      LOGL = -1
      CALL FZLOGL(LUNIT,LOGL)
      CALL FZFILE(LUNIT,0,TEMP)
C
      IF(JOPT) THEN
        CALL DBFZUP(LUNIT, 'O')
        IF(IQUEST(1) .NE. 0) THEN
          CALL ERRDB('ERROR UPDATING FROM JOURNAL FILE')
          IOK = .FALSE.
        ENDIF
      ELSE
        CALL FZIN(LUNIT, IDVSTP, LBANK, 2, ' ', 0, 0)
        IBANK = IC(LBANK - 4)
        CALL STR$TRIM(PATH, PATH, CLEN)
        IF (BNKNAM .NE. PATH(CLEN-3:CLEN)) THEN
          CALL INTMSG(' Inserting into wrong data base path:')
          WRITE(MSG,60)BNKNAM,PATH(CLEN-3:CLEN)
   60     FORMAT(' File contains ',A4,' bank, database is ',A4)
          CALL INTMSG(MSG)
          IOK = .FALSE.
          GO TO 997
        ENDIF
        CALL RZCDIR(PATH(1:CLEN), 'U')
        CALL DBCLB_INSERT(PATH(1:CLEN), IOK)
        IF(.NOT. IOK)THEN
          CALL INTMSG(' Error inserting FZ file in database')
        ENDIF
      ENDIF
C
  997 CONTINUE
      CALL FZENDI(LUNIT,'T')
  998 CONTINUE
      CLOSE(LUNIT)
      CALL RLUNIT(15,LUNIT,ERR)
C
C----------------------------------------------------------------------
  999 RETURN
      END
