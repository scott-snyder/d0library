      SUBROUTINE FCMTCH(PHIC,PHIF,THEC,THEF,ETHC,ETHF,PT,XF,YF,XC,YC,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Match tracks between CDC and FDC
C-
C-   Inputs  : PHIC : Phi of CDC track
C-             PHIF : Phi of FDC track
C-             THEC : Theta of CDC track
C-             THEF : Theta of FDC track
C-             ERRV : error on theta of CDC track
C-             ERRF : error on theta of FDC track
C-             PT   : transverse momentum of a track
C-             XF,YF: the (X,Y,0) point on the FDC track
C-             XC,YC: center of gravity of CDC track
C-   Outputs : OK   : true for matched tracks,
C-                    false for non-matched tracks
C-
C-   Created  13-DEC-1990   Jeffrey Bantly copied from FVMTCH.FOR
C-   Updated   1-AUG-1991   Susan K. Blessing  Put EZRSET in. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IER
C
      REAL    PHIC, PHIF, THEC, THEF, ETHC, ETHF, PT, PTOLD
      REAL    XF, YF, XC, YC, DELPHI2
      REAL    FCRESP, FCTMCS, SGMFCT, DSTFC3, PTLIMT, TOLPH2, TOLDI2
      REAL    DIFTHE, ERRTHE, TOLPHI, TOLDIS, DELPHI, DELTHE, THEERR
C
      LOGICAL OK, ENDSEG, FIRST
C
      DATA FIRST/.TRUE./
      DATA PTOLD/9999.9/
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        CALL EZGET('FCRESP',FCRESP,IER)
        CALL EZGET('FCTMCS',FCTMCS,IER)
        CALL EZGET('SGMFCT',SGMFCT,IER)
        CALL EZGET('DSTFC3',DSTFC3,IER)
        CALL EZGET('PTLIMT',PTLIMT,IER)
        CALL EZGET('TOLPH2',TOLPH2,IER)
        CALL EZGET('TOLDI2',TOLDI2,IER)
        CALL EZGET('DIFTHE',DIFTHE,IER)
        CALL EZGET('ERRTHE',ERRTHE,IER)
        CALL EZRSET
      END IF
C
      IF (PT .NE. PTOLD) THEN
C
C  use tighter cut for higher PT tracks in matching FDC tracks with CDC tracks
C  use loose cut in the matching, when PT of the track is lower than the
C  PT limit. The PT limit and the cuts are defined in ZTRAKS.RCP
C
        IF (PT .GE. PTLIMT) THEN
          TOLPHI = SGMFCT * SQRT(FCRESP**2 + (FCTMCS / PT)**2)
          TOLDIS = TOLPHI * DSTFC3
        ELSE
          TOLPHI = TOLPH2
          TOLDIS = TOLDI2
        ENDIF
        PTOLD = PT
      ENDIF
C
C   phi cut for matching - phi angle between FDC and CDC track must be, within
C   a tolerance, pi.  That is, PHIF and PHIC should be opposite each other
C   given the nature of a track through both chambers.
C
      DELPHI = ABS(PHIF - PHIC)
      IF(PHIF.GT.PHIC) THEN
        DELPHI2 = ABS(PHIF - PHIC - PI)
      ELSE
        DELPHI2 = ABS(PHIC - PHIF - PI)
      ENDIF
      IF (DELPHI .GT. TOLPHI .AND. DELPHI2 .GT. TOLPHI) GO TO 300
C
C   theta cut for matching - theta angle between FDC and CDC track should be
C   either the same or THEF should equal Pi-THEC.
C
      DELTHE = ABS(THEF - THEC)
      IF(DELPHI2.LE.TOLPHI) DELTHE = ABS(PI - THEC -THEF)
      IF (DELTHE .LT. DIFTHE) THEN
        THEERR = SQRT(ETHF**2 + ETHC**2)
        IF (THEERR .GT. ERRTHE) GOTO 300
      ELSE
        GOTO 300
      ENDIF
C
C   cut on the distance between CDC and FDC tracks for matching - ignored for
C   since the distance is up the length of the CDC, hardly a cut.
C
C      OK = ENDSEG(XF,YF,PHIF,XC,YC,PHIC,TOLDIS)
      OK = .TRUE.
C
      GOTO 999
  300 OK = .FALSE.
C-------------------------------------------------------------------------
  999 RETURN
      END
