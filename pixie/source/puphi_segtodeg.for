
      SUBROUTINE PUPHI_SEGTODEG(IPHI,IDPHI,CENPHI,WIDPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts CAL. PHI segment to degree.
C-
C-   Inputs  : IPHI - selected phi index(1-32:up only, 33 is same as 1)
C-                   in the calorimeter END view.
C-             IDPHI- half width of selected phi(0-15).
C-   Outputs : CENPHI - A center of Phi slice in degree( 0=<180 ).
C-             WIDPHI - A half width of Phi slice in degree.
C-   Controls:
C-
C-   Created   1-MAY-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IPHI,IDPHI
      INTEGER ARGSOK
      REAL    CENPHI,WIDPHI
      REAL    PHI,DPHI,SEGPHI
      DATA    SEGPHI /5.625/
C----------------------------------------------------------------------
C-
C--- Get CENPHI and WIDPHI( PX_SYSTEM_RCP requires DEGREES )
C-
      IF(IPHI.LT.1 .OR. IPHI.GT.64) THEN
        CALL ERRMSG('PIXIE','PUPHI_SEGTODEG','Invalid IPHI range(1:32)',
     &    'W')
        GO TO 999
      ENDIF
      IF(IPHI.GT.32) IPHI = IPHI - 32
      IF(IDPHI.GT.15) IDPHI = 15        ! Max. of IDPHI
      CALL CALPHI(IPHI,1,PHI,DPHI,ARGSOK)
      CENPHI = PHI/RADIAN
      CALL CALPHI(IPHI-IDPHI,1,PHI,DPHI,ARGSOK)
      WIDPHI = (PHI-DPHI/2.)/RADIAN
      WIDPHI = CENPHI - WIDPHI
C-
      RETURN
      ENTRY PUPHI_DEGTOSEG(CENPHI,WIDPHI,IPHI,IDPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts degree to CAL. PHI segment.
C-
C-   Input   : CENPHI - A center of Phi slice in degree( 0=<180 ).
C-             WIDPHI - A half width of Phi slice in degree.
C-   Outputs : IPHI - selected phi index(1-32:up only, 33 is same as 1)
C-                   in the calorimeter END view.
C-             IDPHI- half width of selected phi(0-15).
C----------------------------------------------------------------------
C-
C--- Get IPHI and IDPHI
C-
      IF(CENPHI.LT.0. .OR. CENPHI.GE.360.) THEN
        CALL ERRMSG('PIXIE','PUPHI_DEGTSEG',
     &    'Invalid CENPHI range(0=<180)','W')
        GO TO 999
      ENDIF
      IF(CENPHI.GE.180.) CENPHI = CENPHI - 180.
      IF(WIDPHI.GT.84.375) WIDPHI = 84.375      ! Equivalent IDPHI=15
      IPHI = INT(CENPHI/SEGPHI) + 1
      CALL CALPHI(IPHI,1,PHI,DPHI,ARGSOK)
      DPHI   = DPHI/RADIAN
      WIDPHI = WIDPHI + DPHI/2.
      IDPHI  = INT(WIDPHI/SEGPHI)
C-
  999 RETURN
      END
