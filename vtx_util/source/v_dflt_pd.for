      SUBROUTINE V_DFLT_PD(DFLT_PD, DFLT_PD_SIG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VTX pedestal banks with default pedestal values
C-                         from VTRAKS_RCP (used in case reading DBL3 fails).
C-
C-   Inputs  : DFLT_PD, DFLT_PD_SIG: pedestal and sigma from VTRAKS_RCP
C-   Outputs : fills VPDL banks
C-
C-   Created  11-MAY-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      REAL DFLT_PD, DFLT_PD_SIG
C
      INTEGER LAY, NSEC(0:2), NFADC, I, NCHAN
      INTEGER POINT, LVPDL, GZVPDL
      DATA NSEC / 16, 32, 32 /
      PARAMETER ( NFADC = 16 )      ! # OF CHAN PER SECTOR
C----------------------------------------------------------------------
      DO LAY = 0, 2
        LVPDL = GZVPDL(LAY)
        IF ( LVPDL .LE. 0 ) THEN
          CALL ERRMSG('VTRAKS','VDFLTPD',
     &      'VTX STP banks not present, abort','F')
        ENDIF
        NCHAN = NSEC(LAY) * NFADC
        POINT = LVPDL + 5
        DO I = 1, NCHAN
          C(POINT+1) = DFLT_PD
          C(POINT+2) = DFLT_PD_SIG
          POINT = POINT + 2
        ENDDO
      ENDDO
  999 RETURN
      END
