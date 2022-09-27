      SUBROUTINE ZCHECK_VTXT(THETA,THETAV,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if VTX track theta is too far from the
C-                         CDC/FDC track theta
C-                         This check is used to determine if the VTX
C-                         track should contribute to the ZFIT
C-
C-   Inputs  : THETA:  CDC/FDC track's theta
C-             THETAV: VTX track theta
C-   Outputs : OK: true if the theta difference is within the tolerance
C-
C-   Created  18-JUN-1993   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      REAL    THETA, THETAV, DELTHE, CURVE, MINTHE, DIFTHE, THEDIF
      REAL    VTXPRM(3)
      LOGICAL FIRST, OK, EZERROR, NO_BAD_MATCH
      SAVE    FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
      OK = .FALSE.
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZCHECK_VTXT',
     &    'Unable to find bank VTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('THETA_RESOL',VTXPRM(1),IER)
        CALL EZRSET
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZCHECK)VTXT',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MIN_THETA_ROAD',MINTHE,IER)
        CALL EZGET_l('NO_BAD_MATCH',NO_BAD_MATCH,IER)
        CALL EZGET('DIFTHE',DIFTHE,IER)
        CALL EZRSET
      ENDIF
      IF (THETAV .LE. 0.0) GOTO 999
      CURVE = MAX(VTXPRM(2),VTXPRM(1)*SIN(THETA)**2 )
      THEDIF = MAX(CURVE,MINTHE)
      IF (NO_BAD_MATCH) THEN
        THEDIF = MIN(THEDIF,DIFTHE)
      ENDIF
      DELTHE = ABS(THETA - THETAV)
      OK = DELTHE .LE. THEDIF
C
  999 RETURN
      END
