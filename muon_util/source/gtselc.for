      SUBROUTINE GTSELC(ISTA,ISEC,ITUBE,PED,PSIG,COA,COB,CHIS,ANONL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get SAMUS tubes calibration constants (SELC)
C-                         TIME = (ADC - COB) / COA
C-
C-   Inputs  : ISTA  - station number
C-             ISEC  - section number
C-             ITUBE - tube number
C-                     ITUBE = 0   Gives general section information
C-
C-   Outputs : PED   - Pedestal value
C-             PSEG  - Pedestal sigma
C-             COA   - Coefficient A (1/slope)
C-             COB   - Coefficient B (ADC zero time)
C-             CHIS  - Chi Square of linear fit
C-             ANONL - Nonlinearity
C-   Controls:
C-
C-   Created  15-JUN-1995   M. Fortner
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER ISTA, ISEC, ITUBE
      REAL PED, PSIG, COA, COB, CHIS, ANONL
      INTEGER GZSELC, LSELC, ISELC
      EXTERNAL GZSELC
C
C       Get addresses of the calibration bank SELC
C
      PED = -999.0
      COA = 1.0
      COB = 0.0
      LSELC = GZSELC (ISTA,ISEC)
      IF (LSELC.LE.0) RETURN
      IF (ITUBE.GT.IC(LSELC+10)) RETURN
C
      IF (ITUBE.GT.0) THEN
        ISELC = LSELC + 10 + 6*(ITUBE-1)
        PED   = C(ISELC+1)
        PSIG  = C(ISELC+2)
        COA   = C(ISELC+3)
        COB   = C(ISELC+4)
        CHIS  = C(ISELC+5)
        ANONL = C(ISELC+6)
      ELSE
        PED   = FLOAT(IC(LSELC+4))      ! Lowest run number
        PSIG  = FLOAT(IC(LSELC+5))      ! Highest run number
        COA   = FLOAT(IC(LSELC+6))      ! Run generated
        COB   = FLOAT(IC(LSELC+7))      ! Date Generated
        CHIS  = FLOAT(IC(LSELC+9))      ! Module ID number
        ANONL = FLOAT(IC(LSELC+10))     ! Number of channels
      ENDIF
C
  999 CONTINUE
      RETURN
      END
