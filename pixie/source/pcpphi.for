      SUBROUTINE PCPPHI
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pick a PHI segment on the CAL END View
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-
C-   Updated  18-JAN-1992   N. Oshima ( Use 'CAL ONLY' )
C-   Updated  03-JUL-1991   N. Oshima ( SYSTEM PICKING )
C-   Updated  16-MAY-1991   N. Oshima ( Global PHI handling by PX_SYSTEM_RCP )
C-   Modified 23-APR-1991   N. Oshima ( Rename from PUPHI and add EZ stuffs )
C-   Created  13-DEC-1988   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:PXCOMK.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER ICHAR,ICAPHI,IDPHI, IER
      REAL    XP(3), PHI
      REAL    CENPHI,WIDPHI
      CHARACTER*1  ICHOIC
      LOGICAL CONLY,LPHITYP,EZERROR
C-
C----------------------------------------------------------------------
C-
C
C ****  Get window coordinates of picked point
C
      CALL PU_GET_PICKW(XP)
C
C ****  Getting Phi
C
      PHI=ATAN2(XP(2),XP(1))
      IF(PHI.LT.0.) PHI=PHI+TWOPI
      ICAPHI=INT((PHI/TWOPI)*NPHIL)+1
      ICAPHI=MIN(ICAPHI,NPHIL)
C-
C--- Update CAL PHI and CAL DPHI in PX_CALDIS_RCP
C-
      CALL EZPICK('PX_CALDIS_RCP')          ! Selecting CALDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCPPHI','Bank PX_CALDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
      CALL PUGETV('CAL ONLY',CONLY)
      CALL PUGETV('CAL JET DPHI',IDPHI)
      IF ( CONLY ) THEN
        CALL PUSETV('CAL DPHI',IDPHI)
        CALL PUSETV('CAL PHI',ICAPHI)
      ENDIF
C-
      CALL EZRSET
C-
C--- Update PHI CENTER and PHI WIDTH in PX_SYSTEM_RCP for GLOBAL Mode
C-
      IF ( .NOT. CONLY ) THEN
        CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PCPPHI','Bank PX_SYSTEM_RCP NOT FOUND',
     &      'W')
          GOTO 999
        ENDIF
        CALL PUGETV('PHI TYPE',LPHITYP)
        IF(LPHITYP) THEN     ! GLOBAL Mode
C-
C--- Impose up and down region in the D0 SIDE view
C-
          IF(ICAPHI .GT. NPHIL/2) ICAPHI = ICAPHI - NPHIL/2
          CALL PUPHI_SEGTODEG(ICAPHI,IDPHI,CENPHI,WIDPHI)
          CALL PUSETV('PHI CENTER',CENPHI)
          CALL PUSETV('PHI WIDTH',WIDPHI)
        ENDIF
C-
        CALL EZRSET
      ENDIF
C-
  999 RETURN
      END

