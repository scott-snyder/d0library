      SUBROUTINE ZTWO_CLOSE( TRACK1,TRACK2,
     &                       XC1,YC1,ZC1,XC2,YC2,ZC2,RC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For two ZFIT tracks, return the point of
C-   closest aproach.
C-
C-   Inputs  : TRACK1,TRACK2 
C-   Outputs : XC1,YC1,ZC1,XC2,YC2,ZC2,RC
C-   Controls: 
C-
C-   Created  25-FEB-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Input:
      INTEGER TRACK1,TRACK2
C
C  Output:
      REAL    XC1,YC1,ZC1,XC2,YC2,ZC2,RC
C
C  Local:
      INTEGER GZZFIT
      INTEGER LZFIT1 ,LZFIT2 
      REAL    X_1 ,Y_1, Z_1, DX_1 ,DY_1, DZ_1
      REAL    X_2 ,Y_2, Z_2, DX_2 ,DY_2, DZ_2
      REAL    DEL_X ,DEL_Y, DEL_Z
      REAL    A1 ,A2 ,B1 ,B2 ,C  
      REAL    DENOM, T1, T2
C----------------------------------------------------------------------
C
      RC = 99999.
      LZFIT1 = GZZFIT(TRACK1)
      IF ( LZFIT1.LE.0 ) GOTO 999
      LZFIT2 = GZZFIT(TRACK2)
      IF ( LZFIT2.LE.0 ) GOTO 999
C
C  Tracks expressed as point and direction cosines:
C
      X_1 = Q(LZFIT1 + 11) 
      Y_1 = Q(LZFIT1 + 12) 
      Z_1 = Q(LZFIT1 + 15) 
      DX_1 = Q(LZFIT1 + 20)
      DY_1 = Q(LZFIT1 + 22)
      DZ_1 = Q(LZFIT1 + 24)
C
      X_2 = Q(LZFIT2 + 11) 
      Y_2 = Q(LZFIT2 + 12) 
      Z_2 = Q(LZFIT2 + 15) 
      DX_2 = Q(LZFIT2 + 20)
      DY_2 = Q(LZFIT2 + 22)
      DZ_2 = Q(LZFIT2 + 24)
C
      DEL_X = X_1-X_2
      DEL_Y = Y_1-Y_2
      DEL_Z = Z_1-Z_2
C
      A1 =  DX_1*DEL_X + DY_1*DEL_Y + DZ_1*DEL_Z
      A2 = -DX_2*DEL_X - DY_2*DEL_Y - DZ_2*DEL_Z
      B1 = DX_1**2 + DY_1**2 + DZ_1**2
      B2 = DX_2**2 + DY_2**2 + DZ_2**2
      C  = -(DX_1*DX_2 + DY_1*DY_2 + DZ_1*DZ_2)
      DENOM = B1*B2 - C**2
C
      IF ( DENOM.NE.0 ) THEN
        T1 = (A2*C - A1*B2) / DENOM
        T2 = (A1*C - A2*B1) / DENOM
      ELSEIF ((DZ_1.NE.0).AND.(DZ_2.NE.0)) THEN
        T1 = - ZC1 / DZ_1
        T2 = - ZC2 / DZ_2
      ELSE
        T1 = 0.0
        T2 = 0.0
      ENDIF
C
      XC1 = X_1 + DX_1*T1
      YC1 = Y_1 + DY_1*T1
      ZC1 = Z_1 + DZ_1*T1
C
      XC2 = X_2 + DX_2*T2
      YC2 = Y_2 + DY_2*T2
      ZC2 = Z_2 + DZ_2*T2
C
      RC = (XC1-XC2)**2. + (YC1-YC2)**2. + (ZC1-ZC2)**2.
      RC = SQRT(RC)
C
  999 RETURN
      END
