      SUBROUTINE D0DBL3_DELETE(PATH,NKEY,KEY,COPT,KTIME,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-             PATH     RZ path name.
C-             NKEY     Number keys.
C-             KEY      Array of keys. In case of IACT=5, KEY(1:NKEY)
C                        are old keys and KEY(NKEY+1:2*NKEY) are new keys.
C-             COPT     Character option for DBENTER & DBUSE (separate by -)
C-                       or just for DBPURK OR DBPURG.
C-             KTIME    If true then key(3&4) are time otherwise are run #s
C-
C-   Outputs :
C-   Controls: IOK      If .false. then trouble.
C-
C-   Created  16-JUN-1992   SHAHRIAR ABACHI
C-   Modified 24-JUN-1992   SHAHRIAR ABACHI   Modified for case of time key
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH,COPT
      INTEGER NKEY,KEY(NKEY)
      LOGICAL KTIME,IOK,OK
      INCLUDE      'D0$PARAMS:CALIB.DEF'
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$INC:D0DBL3_LNK.INC'
      INCLUDE      'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE      'D0$INC:QUEST.INC'
      INTEGER NMKY
      PARAMETER (NMKY=99)
      INTEGER       KEYO(NMKY),KEYN(NMKY),KEYC(NMKY),KEY2(NMKY)
      INTEGER       KEYPO(NMKY),KEYPN(NMKY),KEY3(NMKY)
      INTEGER       I,LD,LK,LDAT,LKEY
      INTEGER       RUN, SAVE_RUN, US,ICT
      CHARACTER*80  MSG
      CHARACTER*1   CHYN
      LOGICAL       PRERUN,NXTRUN
      INTEGER CLEN,TRULEN
      CHARACTER*30 CPURK,CFREE,CUSE,CH1,CH2
C----------------------------------------------------------------
      CALL VZERO(KEYO,NKEY)
      CALL VZERO(KEYN,NKEY)
      CALL VZERO(KEYC,NKEY)
      CALL VZERO(KEY2,NKEY)
      CALL UCOPY(KEY(1),KEY2(1),NKEY)
C
      PRERUN = .FALSE.
      NXTRUN = .FALSE.
      IOK = .FALSE.
      CUSE = ' '
      CPURK = ' '
      CFREE = ' '
      CLEN = TRULEN(COPT)
      IF(COPT(1:1) .EQ. 'S') THEN
        CPURK = COPT(1:CLEN)
        CUSE = CPURK
        CFREE = 'K'//COPT(1:CLEN)
      ELSE
        CALL ERRDB
     &(' D0DBL3_DELETE: COPT should start with character S')
        GOTO 999
      ENDIF
C
      CALL RZCDIR( PATH, 'U')
      RUN = KEY(3)
      KEY2(4) = KEY2(3)
      CALL DBUSE(PATH,LKEY,LDAT,RUN,KEY2,CUSE)
      KEY2(4) = KEY(4)
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('D0DBL3_DELETE: DBUSE (first call)')
        GO TO 999
      ENDIF
      DO 5 I = 1,NKEY
        KEYO(I) = IC(LKEY + I)
    5 CONTINUE
      CALL INTMSG(' ----------------')
      WRITE(MSG,15) RUN
   15 FORMAT(2x,' Input run Number = ',I9)
      WRITE(MSG,150) KEYO(3)
  150 FORMAT(2x,' Actual run Number to be deleted = ',I9)
      CALL INTMSG(MSG)
      CALL INTMSG(' ----------------')
C
C - Delete run
C
      CALL DBFREE ( PATH, LKEY, KEYO, 'K'//CUSE )
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('D0DBL3_DELETE: DBFREE')
        GO TO 998
      ENDIF
      CALL DBPURK ( PATH, KEYO(3), KEYO, CPURK )
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB
     &      ('D0DBL3_DELETE: DBPURK Error deleting current keys.')
        GO TO 998
      ENDIF
C
C-  find previous run
C
      IF(KTIME) THEN
        CALL D0DBL3_DBINCT(KEYO(3), -1, KEY2(3))
      ELSE
        KEY2(4) = KEYO(3) - 1
      ENDIF
C
      IF ( KEY2(4) .EQ. 0 ) THEN            ! no previous run
        IF(KTIME) THEN
          CALL D0DBL3_DBINCT(KEYO(4), 1, KEY2(3))
        ELSE
          KEY2(3) = KEYO(4) + 1
        ENDIF
        NXTRUN = .TRUE.
        SAVE_RUN = KEYO(3)
      ELSE
        PRERUN = .TRUE.
        SAVE_RUN = KEYO(4)
      ENDIF
C
      IF(PRERUN .OR. NXTRUN) THEN
        IF(PRERUN) KEY2(3) = KEY2(4)
        IF(NXTRUN) KEY2(4) = KEY2(3)
        CALL DBUSE(PATH,LKEY,LDAT,KEY2(3),KEY2,CUSE)
        IF (IQUEST(1) .NE. 0 .OR. LKEY .EQ. 0) THEN
          CALL ERRDB('D0DBL3_DELETE DBUSE.
     &        No previous/next run exist but deletion will go on.')
          PRERUN = .FALSE.
          NXTRUN = .FALSE.
        ELSE
          DO 10 I = 1,NKEY
            KEYPO(I) = IC(LKEY+I)            ! get old keys
            KEYPN(I) = KEYPO(I)              ! name new same as old
   10     CONTINUE
        ENDIF
      ENDIF
C
C - Change previous run
C
      IF (PRERUN .OR. NXTRUN) THEN
        IF(PRERUN) KEYPN(4) = SAVE_RUN
        IF(NXTRUN) KEYPN(3) = SAVE_RUN
        CALL DBRENK(PATH,KEYPO,KEYPN)       ! change keys
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB
     &        ('D0DBL3_DELETE: DBRENK. Error deleting previuos run.')
          CALL DBFREE ( PATH, LKEY, KEYPO, 'K'//CUSE )
          GO TO 998
        ENDIF
        CALL DBFREE ( PATH, LKEY, KEYPN, 'K'//CUSE )
      ENDIF
C
      CALL D0DBL3_FINISH
      ICT = 0
   11 CALL D0DBL3_INIT('DBFILE', OK)
      IF(.NOT. OK) THEN
        ICT = ICT + 1
        IF(ICT .LT. 3) GOTO 11
        PRINT *,
     &    'Server- Problem in dbl3 initialization.'
      ENDIF
C
      IOK = .TRUE.
  998 CONTINUE
      CALL RZCDIR( PATH, 'U')

C----------------------------------------------------------------------
  999 RETURN
      END
