      SUBROUTINE V_DFLT_TM(DFLT_TM, DFLT_TM_SIG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VTX tzero banks with default pedestal values
C-                         from VTRAKS_RCP (used in case reading DBL3 fails).
C-
C-   Inputs  : DFLT_TM, DFLT_TM_SIG: tzero and sigma from VTRAKS_RCP
C-   Outputs : fills VTMW banks
C-
C-   Created  11-MAY-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL DFLT_TM, DFLT_TM_SIG
C
      INTEGER LAY, NSEC(0:2), NWIRE, I, NCHAN
      INTEGER POINT, LVTMW, GZVTMW, IVERS
      DATA NSEC / 16, 32, 32 /
      PARAMETER ( NWIRE = 8 )
C----------------------------------------------------------------------
      DO LAY = 0, 2
        LVTMW = GZVTMW(LAY)
        IF ( LVTMW .LE. 0 ) THEN
          CALL ERRMSG('VTRAKS','VDFLTTM',
     &      'VTX STP banks not present, abort','F')
          GO TO 999
        ENDIF
        IVERS = IBITS(IC(LVTMW),13,5)   ! BANK VERSION #
        IF ( IVERS .LE. 0 ) THEN
          CALL ERRMSG('VTRAKS','VDFLTTM',
     &      'Wrong version of VTMW, abort','F')
          GO TO 999
        ENDIF
        NCHAN = NSEC(LAY) * NWIRE
        POINT = LVTMW + 5
        DO I = 1, NCHAN
          C(POINT+1) = DFLT_TM      ! End 0
          C(POINT+2) = DFLT_TM_SIG
          C(POINT+3) = DFLT_TM      ! End 1
          C(POINT+4) = DFLT_TM_SIG
          POINT = POINT + 5
        ENDDO
      ENDDO
  999 RETURN
      END
