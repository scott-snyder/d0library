      SUBROUTINE D0DBL3_MODKEY(PATH,NKEY,KEY,COPT,KTIME,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-             PATH     RZ path name.
C-             NKEY     Number keys x 2 . Old keys + New keys
C-             KEY      Array of keys. (old and new)
C-             COPT     Character option for DBUSE
C-             KTIME    if true then KEY(3&4) are time otherwise are run #s.
C-
C-   Outputs :
C-   Controls: IOK      If .false. then trouble.
C-
C-   Created   16-JUN-1992   SHAHRIAR ABACHI
C-   Modified  24-JUN-1992   SHAHRIAR ABACHI  Modified for case of time keys
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH, COPT
      INTEGER NKEY,KEY(NKEY)
      LOGICAL KTIME,IOK
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:D0DBL3_LNK.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER LKEYS, LDATA
      CHARACTER*80 MSG
      CHARACTER*30 CUSE
      INTEGER I,NEWRUN,IERR,CLEN,PRUN,NRUN,NMKY
      PARAMETER (NMKY=99)
      INTEGER KEYC(NMKY),KEYN(NMKY),KEYP(NMKY),KEYNM(NMKY)
      INTEGER KEYC_OLD(NMKY),KEYN_OLD(NMKY),KEYP_OLD(NMKY)
      INTEGER KEYPM(NMKY),KEY2(NMKY),KEYCM(NMKY),IDUM,TRULEN
      LOGICAL PRERUN,NXTRUN
C
C----------------------------------------------------------------------
      IOK = .FALSE.
      PRERUN = .FALSE.
      NXTRUN = .FALSE.
      PRUN = 0
      NRUN = 0
      CUSE = ' '
      CALL VZERO(KEYP(1),NKEY)
      CALL VZERO(KEYC(1),NKEY)
      CALL VZERO(KEYN(1),NKEY)
      CALL VZERO(KEY2(1),NKEY)
      CALL UCOPY(KEY(1),KEY2(1),NKEY)
C
      CLEN = TRULEN(COPT)
      IF(COPT(1:2) .EQ. 'KS') THEN
        CUSE = COPT(1:CLEN)
      ELSE
        CALL ERRDB
     &(' D0DBL3_MODKEY: COPT should start with character KS')
        GOTO 999
      ENDIF
C
      CALL RZCDIR(PATH, 'U')
C
C - Current run
C
      KEY2(4) = KEY2(3)
      CALL DBUSE(PATH,LKEYS,LDATA,KEY(3),KEY2,CUSE)
      KEY2(4) = KEY(4)
      IF(IQUEST(1) .NE. 0 .OR. LKEYS .EQ. 0) THEN
        CALL ERRDB
     &    (' D0DBL3_MODKEY: Problem with DBUSE. No modification done')
        GOTO 998
      ENDIF
C
      DO I = 1,NKEY/2
        KEYC(I) = IC(LKEYS + I)
        KEYC_OLD(I) = KEYC(I)
      ENDDO
C
C- Previous run
C
      IF (KEY(3) .NE. KEY(NKEY/2+3)) THEN
        IF (KEYC(3) .GT. 1) THEN
          IF(KTIME) THEN
            CALL D0DBL3_DBINCT(KEYC(3), -1, PRUN)
          ELSE
            PRUN = KEYC(3) - 1
          ENDIF
          KEYC(3) = PRUN
          IDUM = KEYC(4)
          KEYC(4) = KEYC(3)
          CALL DBUSE(PATH,LKEYS,LDATA,PRUN,KEYC,CUSE)
          KEYC(4) = IDUM
          KEYC(3) = KEYC_OLD(3)
          IF(IQUEST(1) .NE. 0 .OR. LKEYS .EQ. 0) THEN
            CALL
     &ERRDB(' D0DBL3_MODKEY: DBUSE. No previous run. Not to worry.')
            PRERUN = .FALSE.
          ELSE
            PRERUN = .TRUE.
            DO I=1,NKEY/2
              KEYP(I) = IC(LKEYS + I)
              KEYP_OLD(I) = KEYP(I)
            ENDDO
            WRITE(MSG,23) KEYP(3),KEYP(4)
   23       FORMAT(' Previous calibration run validity is '
     &          ,I10,' to ',I10)
            CALL INTMSG(MSG)
          ENDIF
        ELSE
          PRERUN = .FALSE.
          CALL INTMSG(' No previous run in database')
        ENDIF
      ENDIF
C
C- Next run
C
      IF ( KEY(4) .NE. KEY(NKEY/2 + 4) ) THEN
        IF (KEYC(4) .NE. 999999999) THEN
          IF(KTIME) THEN
            CALL D0DBL3_DBINCT(KEYC(4), 1, NRUN)
          ELSE
            NRUN = KEYC(4) + 1
          ENDIF
          KEYC(4) = NRUN
          KEYC(3) = NRUN
          CALL DBUSE(PATH,LKEYS,LDATA,NRUN,KEYC,CUSE)
          KEYC(3) = KEYC_OLD(3)
          KEYC(4) = KEYC_OLD(4)
          IF(IQUEST(1) .NE. 0 .OR. LKEYS .EQ. 0) THEN
            CALL ERRDB
     &        (' D0DBL3_MODKEY: DBUSE. No next run. Not to worry.')
            NXTRUN = .FALSE.
          ELSE
            NXTRUN = .TRUE.
            DO I=1,NKEY/2
              KEYN(I) = IC(LKEYS + I)
              KEYN_OLD(I) = KEYN(I)
            ENDDO
            IF (KEYN(4) .NE. 0) THEN
              WRITE(MSG,24)KEYN(3),KEYN(4)
   24         FORMAT(' Next calibration run valididty is '
     &            ,I10,' to ',I10)
              CALL INTMSG(MSG)
            ELSE
              NXTRUN = .FALSE.
              CALL INTMSG(' No next run in database')
            ENDIF
          ENDIF
        ELSE
          NXTRUN = .FALSE.
          CALL INTMSG(' No next run in database')
        ENDIF
      ENDIF
C
      DO I=1,NKEY/2
        KEYCM(I) = KEY(NKEY/2  + I)
        KEYNM(I) = KEYN_OLD(I)
        KEYPM(I) = KEYP_OLD(I)
      ENDDO
      IF(KTIME) THEN
        CALL D0DBL3_DBINCT(KEY(NKEY/2 + 3), - 1, KEYPM(4))
        CALL D0DBL3_DBINCT(KEY(NKEY/2 + 4), 1, KEYNM(4))
      ELSE
        KEYPM(4) = KEY(NKEY/2 + 3) - 1
        KEYNM(3) = KEY(NKEY/2 + 4) + 1
      ENDIF

C
      IF(PRERUN) THEN
        CALL DBRENK(PATH,KEYP_OLD,KEYPM)
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBRENK:  Error modifying keys')
          CALL DBFREE(PATH, LKEYS, KEYP_OLD, 'K')
          GO TO 998
        ENDIF
        CALL DBFREE(PATH, LKEYS, KEYPM, 'K')
      ENDIF
C
      CALL DBRENK(PATH,KEYC_OLD,KEYCM)
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBRENK:  Error modifying keys')
        CALL DBFREE(PATH, LKEYS, KEYC_OLD, 'K')
        GO TO 998
      ENDIF
      CALL DBFREE(PATH, LKEYS, KEYCM, 'K')
C
      IF(NXTRUN) THEN
        CALL DBRENK(PATH,KEYN_OLD,KEYNM)
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBRENK:  Error modifying keys')
          CALL DBFREE(PATH, LKEYS, KEYN_OLD, 'K')
          GO TO 998
        ENDIF
        CALL DBFREE(PATH, LKEYS, KEYNM, 'K')
      ENDIF
C
      IOK = .TRUE.
C
  998 CALL RZCDIR(PATH, 'U')
C
  999 RETURN
      END
