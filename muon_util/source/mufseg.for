      SUBROUTINE MUFSEG(NHIT, XFIT, XSUB, YFIT, SIGYFIT, AY, Y0, D,
     &  CHI2)
C***************************************************************************
C
C       This routine fits NHIT points with coordinates (xfit,yfit) by
C       straight line y=ay*x+y0. SIGYFIT- sigmas of Y.
C       D- is error matrix for parameters ay and y0.
C       The least squares methord is used.
C       INPUT:    NHIT, XFIT, YFIT, SIGYFIT
C       OUTPUT:   AY, Y0, D
C
C
C
C       Created 06-MAR-1993  by Regina Demina.
C-   Updated  23-SEP-1993   Daria Zieminska  fix the sign of the off-diagonal
C-   error matrix element 
C
C***************************************************************************
      IMPLICIT NONE
C       XFIT - x-coordinates,
C       YFIT - y-coordinates,
C       SIGYFIT - their sigmas,
C
      INTEGER NHIT_MAX, NHIT
      PARAMETER(NHIT_MAX=32)
      REAL XFIT(NHIT_MAX), YFIT(NHIT_MAX), SIGYFIT(NHIT_MAX), XSUB
C       NP - number of points on the track.
      REAL Y0, AY, D(2,2), CHI2
      INTEGER NP
      REAL SUM_X, SUM_X2, SUM_XY, SUM_SIGY, SUM_Y
      REAL X, Y, SIGY, B1, B2, DET
      INTEGER IHIT
C
  100 SUM_X2 = 0.
      SUM_X = 0.
      SUM_SIGY = 0.
      SUM_XY = 0.
      SUM_Y = 0.
      NP = 0
      DO IHIT = 1,NHIT
        Y = YFIT(IHIT)
        X = XFIT(IHIT) - XSUB
        SIGY = SIGYFIT(IHIT)
        IF(SIGY.NE.0)THEN
          SUM_X2 = SUM_X2 + X**2/SIGY**2
          SUM_X = SUM_X + X/SIGY**2
          SUM_XY = SUM_XY + Y*X/SIGY**2
          SUM_Y = SUM_Y + Y/SIGY**2
          SUM_SIGY = SUM_SIGY + 1./SIGY**2
          NP = NP + 1
        ENDIF
      ENDDO
C
      IF (NP.GT.0) THEN
        DET = SUM_X2*SUM_SIGY - SUM_X**2
C
        IF (DET.NE.0.) THEN
C
          B1 = SUM_XY*SUM_X - SUM_Y*SUM_X2
          B2 = SUM_XY*SUM_SIGY - SUM_X*SUM_Y
          Y0 = -B1/DET
          AY = B2/DET
          D(1,1) = SUM_SIGY/DET
          D(1,2) = - SUM_X/DET
          D(2,1) = D(1,2)
          D(2,2) = SUM_X2/DET
C
        ENDIF
      ENDIF
C
      CHI2 = 0.
C
      DO IHIT = 1,NHIT
        Y = YFIT(IHIT)
        X = XFIT(IHIT) - XSUB
        SIGY = SIGYFIT(IHIT)
        IF(SIGY.NE.0)THEN
          CHI2 = CHI2 + (Y-AY*X-Y0)**2/SIGY**2
        ENDIF
      ENDDO
  999 RETURN
      END
