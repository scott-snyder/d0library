      SUBROUTINE DBCLB_ENTER(PATH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Entering an FZ file into the data base
C-
C-   Inputs  : PATH - DBL3 path name
C-   Outputs :
C-   Controls:
C-
C-   Created  24-FEB-1990   Srini Rajagopalan
C-   Modified 15-JUN-1992   S. Abachi   Provisions for D0 FZ file out put added
C-                                      to be used in conjunction with server
C-                                      (D option)
C-   Modified 24-SEP-1992   S. Abachi   Server D option was moved to
C-                                      dbclb_insert.for routine.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
C
      CHARACTER*(*) PATH
      CHARACTER*4 BNKNAM
      CHARACTER*40 FILNAM
      CHARACTER*80 MSG
      LOGICAL IOK
      INTEGER LUN,ERR,IOS,IBANK,LBANK,LPATH
C
      EQUIVALENCE(CALIB_LNK(1),LBANK)
      EQUIVALENCE(IBANK,BNKNAM)
C----------------------------------------------------------------------
C
      CALL GETPAR(1,' Enter input file name >','C',FILNAM)
      IF (FILNAM.EQ.' ') THEN
        CALL INTMSG(' Procedure Aborted. No file selected')
        GO TO 999
      ENDIF
c
      CALL GTUNIT(12,LUN,ERR)
      OPEN(UNIT=LUN,FILE=FILNAM,STATUS='OLD',FORM='UNFORMATTED',
     &      IOSTAT=IOS, READONLY )
      IF(IOS.NE.0)THEN
        MSG = 'Error opening file '//FILNAM
        CALL INTMSG(MSG)
        GO TO 999
      ENDIF
      CALL FZFILE(LUN,0,'I')
      CALL FZLOGL(LUN,-2)
      CALL FZIN(LUN,IDVSTP,LBANK,2,' ',0,0)
      CALL FZENDI(LUN,'T')
      CLOSE(LUN)
      CALL RLUNIT(12,LUN,ERR)
      IBANK = IC(LBANK-4)
      CALL STR$TRIM(PATH,PATH,LPATH)
      IF (BNKNAM.NE.PATH(LPATH-3:LPATH)) THEN
        CALL INTMSG(' Inserting into wrong data base path:')
        WRITE(MSG,60)BNKNAM,PATH(LPATH-3:LPATH)
   60   FORMAT(' File contains ',A4,' bank, database is ',A4)
        CALL INTMSG(MSG)
        GO TO 999
      ENDIF
      CALL DBCLB_INSERT(PATH,IOK)
      IF(.NOT.IOK)THEN
        CALL INTMSG(' Error inserting file in data base')
      ENDIF
C
  999 RETURN
      END
