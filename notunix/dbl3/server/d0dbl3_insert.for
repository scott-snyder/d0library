      SUBROUTINE D0DBL3_INSERT(PATH,NKEY,KEY,COPT,LINK,KTIME,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Insert a D0 FZ journal file in database
C-
C-   Inputs  :
C-             PATH     RZ path name.
C-             NKEY     Number keys.
C-             KEY      Array of keys. In case of IACT=5, KEY(1:NKEY)
C                       are old keys and KEY(NKEY+1:2*NKEY) are new keys.
C-             COPT     Character option for DBENTER & DBUSE (separate by -)
C-                      or just for DBPURK OR DBPURG.
C-             LINK     Link to zebra bank or structure if any.
C-             KTIME    If true then key(3&4) are time keys otherwise run #s.
C-
C-   Outputs :
C-   Controls: IOK      If .false. then trouble.
C-
C-
C-   Created   16-JUN-1992   SHAHRIAR ABACHI
C-   Modified  16-JUN-1992   SHAHRIAR ABACHI  Modified for case of time keys
C-   Updated     Jul-1993   J.Green    fix to find entry which has later
C-                                  validity range than the one to be inserted
C-   Updated   20-Jul-1993   S. Abachi  CHOPT for above fix extracted from COPT
C-                                      & time keys correctly handeled
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NKEY,KEY(NKEY),LINK
      CHARACTER*(*) PATH,COPT
      LOGICAL KTIME,IOK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0DBL3_LNK.INC'
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER LBANK,CLEN,TRULEN,M,NMKY,CLEN1,CLEN2,CLEN3,CLEN4
      PARAMETER (NMKY=99)
      INTEGER KEYO(NMKY),KEYN(NMKY),KEYC(NMKY),KEY2(NMKY)
      INTEGER I,LD,LK,LDAT,LKEY,FSTRUN,I3
      CHARACTER*30 CENTR,CPURK,CUSE,CUSE2
      CHARACTER*80  MSG
      LOGICAL OK,DUPLICATE,IEMPTY
      EQUIVALENCE (LBANK,D0DBLNK)
C
C----------------------------------------------------------------------
      CALL VZERO(KEYO,NKEY)
      CALL VZERO(KEYN,NKEY)
      CALL VZERO(KEYC,NKEY)
      CALL VZERO(KEY2,NKEY)
      CALL UCOPY(KEY(1),KEY2(1),NKEY)
c
      I3 = 0
      LBANK = LINK
      IOK = .FALSE.
      CLEN = TRULEN(COPT)
      M = INDEX(COPT, '-')
      IF(M .EQ. 0) THEN
        CALL INTMSG(' WRONG COPT')
        GOTO 999
      ELSE
        CENTR = COPT(1:M-1)
        CUSE = COPT(M+1:CLEN)
        CPURK = COPT(M+2:CLEN)
        CLEN1 = TRULEN(CENTR)
        CLEN2 = TRULEN(CUSE)
        CLEN3 = TRULEN(CPURK)
        I3 = INDEX(CUSE, '3')
        CUSE2 = CUSE(1:I3-1)//CUSE(I3+1:CLEN2)
        CLEN4 = CLEN2 - 1
      ENDIF
C
      IEMPTY = .FALSE.
      DUPLICATE = .FALSE.
C
      DO I=1,NKEY
        KEYC(I) = KEY(I)
      ENDDO
C
      CALL RZCDIR( PATH, 'U')
      KEYC(4) = KEYC(3)
      CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEYC,CUSE(1:CLEN2))
C
      IF (IQUEST(1) .NE. 0 .AND. IQUEST(1) .NE. 24) THEN
        CALL ERRDB('DBUSE')
        IOK = .FALSE.
        GO TO 999
      ELSEIF(I3 .GT. 0 .AND.
     &          (LKEY .EQ. 0 .OR. IQUEST(1) .EQ. 24)) THEN
C                                           try to find entry with later run
        CALL DBUSE(PATH,LKEY,LDAT,KEY(3),KEYC,CUSE2(1:CLEN4))
        IF (IQUEST(1) .NE. 0 .AND. IQUEST(1) .NE. 24) THEN
          CALL ERRDB('DBUSE')
          IOK = .FALSE.
          CALL DBCLB_FINISH
          GO TO 999
        ELSEIF(LKEY .EQ. 0 .OR. IQUEST(1) .EQ. 24) THEN
          WRITE(MSG,11) IQUEST(1),KEY(8)
   11     FORMAT(' IQUEST(1) =',I3,
     &      ' signifies no previous entry for crate ',I10,
     &      '. Not to worry.')
          CALL INTMSG(MSG)
          IEMPTY = .TRUE.
          GOTO 21
        ELSE                            ! found something later
C                                     search through linear struc. to find
C                                     earliest run
          FSTRUN = 999999999
          DO WHILE (LKEY.GT.0)
            FSTRUN = MIN(IC(LKEY+3),FSTRUN)
            LKEY = LC(LKEY)
          ENDDO
          IF (FSTRUN.GT.KEY(3)) THEN
            IF(KTIME) THEN
              CALL D0DBL3_DBINCT(FSTRUN, -1, KEY(4))
            ELSE
              KEY(4) = FSTRUN - 1     ! fix end validity of run being entered
            ENDIF
            GO TO 21
          ELSE
            WRITE(MSG,22) FSTRUN,KEY(8)
   22       FORMAT (' DBUSE failed to find run ',I9,' for crate/mod ',
     &                I5 )
            CALL INTMSG(MSG)
            CALL DBCLB_FINISH
            GOTO 999
          ENDIF
        ENDIF
      ENDIF
C
      DO I = 1,NKEY
        KEYO(I) = IC(LKEY + I)
        KEYN(I) = KEYO(I)
      ENDDO
C
      KEY2(4) = KEYO(4)
C
      IF (KEYN(3) .NE. KEY(3)) THEN
        IF(KTIME) THEN
          CALL D0DBL3_DBINCT(KEY2(3), -1, KEYN(4))
        ELSE
          KEYN(4) = KEY2(3) - 1
        ENDIF
      ELSE
        DUPLICATE = .TRUE.
      ENDIF
C
      IF (DUPLICATE) THEN
        CALL DBFREE ( PATH, LKEY, KEYN, CUSE(1:CLEN2) )
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('D0DBL3_DELETE: DBFREE')
          GO TO 999
        ENDIF
        CALL DBPURK ( PATH, KEYN(3), KEYN, CPURK(1:CLEN3) )
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('D0DBL3_DELETE: DBPURK')
          GO TO 999
        ENDIF
        WRITE(MSG,100)KEYN(3)
  100   FORMAT(' Duplicate Run ',I9,' deleted from database ')
        CALL INTMSG(MSG)
      ELSE
        CALL DBRENK(PATH,KEYO,KEYN)
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBRENK:  Error modifying keys')
          IOK = .FALSE.
          GO TO 999
        ENDIF
        CALL DBFREE(PATH, LKEY, KEYO, 'K')
      ENDIF
C
   21 CONTINUE
C
      CALL DBENTR(PATH,LK,LD,IDVSTP,LBANK,NKEY,KEY2,0,CENTR(1:CLEN1))
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBENTR:  Error in entering data in data base')
        IOK = .FALSE.
        IF(.NOT. IEMPTY) THEN
          CALL DBRENK(PATH,KEYN,KEYO)
          IF (IQUEST(1).NE.0) THEN
            CALL ERRDB('DBRENK:  Error re-modifying keys')
          ENDIF
        ENDIF
        GO TO 999
      ENDIF
C
      CALL DBFREE(PATH, LKEY, KEY2, 'K')
C
      IOK = .TRUE.
      CALL RZCDIR( PATH, 'U')            ! make it visible
C
      IOK = .TRUE.
C
  999 CONTINUE
      RETURN
      END
