      SUBROUTINE D0DBL3_UPDATE(IACT,PATH,NKEY,KEY,COPT,LINK,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To update database from D0 FZ journal file.
C-
C-   Inputs  :
C-             IACT     Action to take. 10=Update,3=Delete,5=Modify.
C-             PATH     RZ path name.
C-             NKEY     Number keys.
C-             KEY      Array of keys. In case of IACT=5, KEY(1:NKEY)
C                       are old keys and KEY(NKEY+1:2*NKEY) are new keys.
C-             COPT     Character option for DBENTER & DBUSE (separate by -)
C-                      or just for DBPURK OR DBPURG.
C-             LINK    Link to zebra bank or structure if any.
C-
C-   Outputs :
C-   Controls: IOK      If .false. then trouble.
C-
C-   Created   16-JUN-1992   SHAHRIAR ABACHI
C-   Modified  15-DEC-1993   SHAHRIAR ABACHI     Cleanup
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IACT,NKEY,KEY(NKEY),LINK
      CHARACTER*(*) PATH,COPT
      LOGICAL IOK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0DBL3_LNK.INC'
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER LUNIT, ERR, LOGL, IOS, LBANK, IBANK, CLEN
      CHARACTER*80 MSG
      LOGICAL KTIME
      INTEGER IT,IACT2,TRULEN
      EQUIVALENCE (LBANK,D0DBLNK)
C
      LBANK = LINK
      IOK = .TRUE.
C
      CLEN = TRULEN(PATH)
C
      IT = MOD(IACT,10)
      IF(IT .EQ. 0) THEN
        KTIME = .FALSE.
      ELSE
        KTIME = .TRUE.
      ENDIF
      IACT2 = IACT / 10
C
      IF(IACT2 .EQ. 1) THEN
        CALL D0DBL3_INSERT(PATH,NKEY,KEY,COPT,LBANK,KTIME,IOK)
      ELSEIF(IACT2 .EQ. 3) THEN
        CALL D0DBL3_DELETE(PATH,NKEY,KEY,COPT,KTIME,IOK)
      ELSEIF(IACT2 .EQ. 5) THEN
        CALL D0DBL3_MODKEY(PATH,NKEY,KEY,COPT,KTIME,IOK)
      ENDIF
C
      IF(.NOT. IOK)THEN
        IF(IACT2 .EQ. 1)
     &CALL INTMSG(' Error inserting FZ file in database')
        IF(IACT2 .EQ. 3)
     &CALL INTMSG(' Error deleting item from database')
        IF(IACT2 .EQ. 5)
     &CALL INTMSG(' Error modifying keys in database')
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
