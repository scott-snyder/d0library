      SUBROUTINE ZDB_INSERT(BKNAME,IZLINK,PATH,KEY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Insert STP bank directly into DBL3 database
C-
C-   Inputs  : BKNAME = Top level Bank to be inserted into database for 
C-             IZLINK = 0. If IZLINK is non zero, the top level bank will
C-             be the link under BKNAME. 
C-             IZLINK = 0 if BKNAME is the top level bank, else specifies
C-             link number under the top level bank to be inserted.
C-             PATH = DBL3 Path name where the data is to be inserted
C-             KEY  = Associated keys with the data
C-   Outputs : IER  = Zero if no errors encountered.
C-   Controls: 
C-
C-   Created   3-OCT-1992   Srini Rajagopalan, Based on dbclb_insert
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C
      INTEGER IZLINK
      INTEGER I,IER,NKYS
      PARAMETER (NKYS=15)
C
      INTEGER KEY(NKYS),KEYC(NKYS)
      INTEGER KEYO(NKYS),KEYN(NKYS)
      INTEGER LKEY,LDAT,LK,LD
      INTEGER HBANK,LBANK,LZFIDH
C
      LOGICAL IEMPTY,DUPLICATE
C
      CHARACTER*4 BKNAME
      CHARACTER*25 PATH
      CHARACTER*80 MSG
C
C----------------------------------------------------------------------
C
      CALL RZCDIR(PATH,' ')
      IF (IQUEST(1).NE.0) THEN
        IER = -1
        CALL ERRMSG('PATH_ERROR','ZDB_INSERT',
     &    'Specified path does not exist in the database','W')
        GO TO 999
      ENDIF
C
      KEYC(3) = KEY(3)
      KEYC(4) = KEY(3)
      KEYC(8) = KEY(8)
C
      CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEYC,'KS348')
C
      IEMPTY = .FALSE.
      IF (IQUEST(1).NE.0 .AND. IQUEST(1).NE.24) THEN
        IER = -2
        CALL ERRMSG('FETCH_ERROR','ZDB_INSERT',
     &    'Unable to fetch previous database entry','W')
        GO TO 999
      ELSE IF (LKEY.EQ.0 .OR. IQUEST(1).EQ.24) THEN
        IEMPTY = .TRUE.       ! No prev. entry in database
        GO TO 200
      ENDIF
C
      DO 10 I = 1,NKYS
        KEYO(I) = IC(LKEY+I)
        KEYN(I) = KEYO(I)
   10 CONTINUE
C
      IF (KEYN(3).NE.KEY(3)) THEN
        KEYN(4) = KEY(3) - 1
        DUPLICATE = .FALSE.
      ELSE
        DUPLICATE = .TRUE.
      ENDIF
C
      KEY(4) = KEYO(4)        ! Overwrite end validity of current run
C
C If duplicate run, delete previous run.. else change keys of previous run
C
      IF (DUPLICATE) THEN
        CALL DBFREE(PATH, LKEY, KEYN, 'KS348')
        IF (IQUEST(1).NE.0) THEN
          IER = -3
          CALL ERRMSG('DBFREE_ERROR','ZDB_INSERT',
     &      'Error while freeing old database entry','W')
          CALL DBCLB_FINISH
          GO TO 999
        ENDIF
C
        CALL DBPURK(PATH, KEYN(3), KEYN, 'S348')
        IF (IQUEST(1).NE.0) THEN
          IER = -4
          CALL ERRMSG('PURGE_ERROR','ZDB_INSERT',
     &      'Error during database purge','W')
          CALL DBCLB_FINISH
          GO TO 999
        ENDIF
C
        WRITE(MSG,100)KEYN(3)
  100   FORMAT(' Duplicate run ',I9,' deleted from database')
        CALL INTMSG(MSG)
C
      ELSE
        CALL DBRENK(PATH,KEYO,KEYN)
        IF (IQUEST(1).NE.0) THEN
          IER = -5
          CALL ERRMSG('KEY_RENAME_ERROR','ZDB_INSERT',
     &      'Error while modifying previous entry keys','W')
          CALL DBCLB_FINISH
          GO TO 999
        ENDIF
      ENDIF
C
  200 CONTINUE
C
C Determine the top level bank address and insert into database
C
      CALL UCTOH(BKNAME,HBANK,4,4)
      LBANK = LZFIDH(IDVSTP,HBANK,0)
      IF (LBANK.GT.0 .AND.IZLINK.GT.0) LBANK = LC(LBANK - IZLINK)
      IF (LBANK.LE.0) THEN
        IER = -6
        CALL ERRMSG('BANK_NOT_FOUND','ZDB_INSERT',
     &    'Input bank name not found','W')
        GO TO 300
      ENDIF
C
      CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKYS,KEY,0,'R')
  300 IF (IQUEST(1).NE.0) THEN
        IER = -6
        CALL ERRMSG('INSERT_ERROR','ZDB_INSERT',
     &    'Error during insertion into database','W')
        IF (.NOT.IEMPTY) THEN
          CALL DBRENK(PATH,KEYN,KEYO)
          IF (IQUEST(1).NE.0) THEN
            CALL ERRMSG('Error rechanging keys','ZDB_INSERT',
     &        'Key structure in database may be corrupted','W')
          ELSE
            CALL ERRMSG('Keys modified','ZDB_INSERT',
     &        'Keys set back to old state successfully','W')
          ENDIF
        ENDIF
        CALL DBCLB_FINISH
        GO TO 999
      ENDIF
      CALL RZCDIR(PATH,'U')
      IER = 0
C
  999 RETURN
      END
