      SUBROUTINE L2CDHT_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get constants needed for CD hit finding
C-                         in level 2 (L2CDHT)
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-MAY-1993   Chris Klopfenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$inc:zebstp.inc'
      CHARACTER*32 bkname(20)
      CHARACTER*50 filename
      INTEGER nbanks, lun, ier, lsl2h, gzsl2h
      INTEGER l2cdht_user,recsiz
      DATA l2cdht_user /666/
      LOGICAL ok, ezerr, open_ok, FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C initialize STP if it hasn't been done already
C
      IF (lstph .LE. 0) CALL INZSTP
      IF (lstph .LE. 0) THEN
        CALL errmsg(' Cannot initialize ZEBSTP common', 
     &    'L2CDHT_INIT', 'Init fail', 'W')
        ok = .false.
        GOTO 999
      ENDIF
C  read RCP file
      filename = 'L2CDHT_RCP'
C
   50 CONTINUE
C
      CALL GTUNIT(l2cdht_user, lun, ier)
      IF (ier .NE. 0) THEN
        CALL errmsg(' GTUNIT failure', 'L2CDHT_INIT',
     &    'RCP fail', 'W')
        ok = .false.
        GOTO 999
      ENDIF
      CALL D0OPEN(lun, filename, 'IF', open_ok)
      IF (.NOT. open_ok) THEN
        CALL errmsg(' RCP file open failure', 'L2CDHT_INIT',
     &    'RCP fail', 'W')
        ok = .false.
        GOTO 999
      ENDIF
C
C read in RCP bank, chain together and hang below L2 STP header (SL2H)
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL BKSL2H(LSL2H)
        IF (LSL2H .LE. 0) THEN
          CALL errmsg(' Unable to book SL2H', 'L2CDHT_INIT',
     &      'SL2H fail', 'W')
          ok = .false.
          GOTO 999
        ENDIF
      ENDIF
      recsiz = 8  !squash out comments
      CALL EZMAKE(lun, recsiz, bkname, nbanks)
      IF (EZERR(ier)) THEN
        CALL errmsg(' RCP read failed', 'L2CDHT_INIT',
     &    'RCP fail', 'W')
        ok = .false.
        GOTO 999
      ENDIF
C
C  add banks to other SRCP banks to be downloaded
C
      CALL L2J_RCP_CHAIN(nbanks, bkname, ier)
      IF (ier .NE. 0) THEN
        CALL errmsg(' L2J_RCP_CHAIN failed', 'L2CDHT_INIT',
     &    'RCP fail', 'W')
        ok = .false.
        GOTO 999
      ENDIF
C
      CLOSE(lun)
      CALL RLUNIT(l2cdht_user, lun, ier)
      IF (ier .NE. 0) THEN
        CALL errmsg(' RLUNIT failed', 'L2CDHT_INIT',
     &    'RCP fail', 'W')
        ok = .false.
        GOTO 999
      ENDIF
C
      IF ( FILENAME.EQ.'L2CDHT_RCP') THEN
        FILENAME = 'CD_HITFIND_RCP'
        GOTO 50
      ENDIF
C
C Pick up VTRAKS_RCP and pedestals for VTX hitfinding
C
      IF ( FILENAME.EQ.'CD_HITFIND_RCP') THEN
        FILENAME = 'VTRAKS_RCP'
        GOTO 50
      ENDIF
C
      CALL L2_VTXINIT
      CALL L2VTX_STP
C
      ok = .true.
  999 RETURN
      END
