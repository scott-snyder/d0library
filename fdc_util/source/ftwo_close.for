      SUBROUTINE FTWO_CLOSE( TRACK1,TRACK2,
     &                       XC1,YC1,ZC1,XC2,YC2,ZC2,RC )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For two FDC tracks, return the point of
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
      INTEGER GZFDCT
      INTEGER LFDCT1 ,LFDCT2 
      REAL    Z0
      REAL    X_1 ,Y_1,DX_1 ,DY_1 
      REAL    X_2 ,Y_2,DX_2 ,DY_2 
      REAL    DEL_X ,DEL_Y 
      REAL    A1 ,A2 ,B1 ,B2 ,C  
      REAL    DENOM       
C----------------------------------------------------------------------
C
      RC = 99999.
      LFDCT1 = GZFDCT(TRACK1)
      IF ( LFDCT1.LE.0 ) GOTO 999
      LFDCT2 = GZFDCT(TRACK2)
      IF ( LFDCT2.LE.0 ) GOTO 999
C
C X_i, Y_i AT Z = 0
C
      CALL FGETZ0(TRACK1,Z0)
      DX_1 = Q(LFDCT1 + 7)
      DY_1 = Q(LFDCT1 + 8)
      X_1 = Q(LFDCT1 + 4) + DX_1*(0.0-Z0) 
      Y_1 = Q(LFDCT1 + 5) + DY_1*(0.0-Z0) 
C
      CALL FGETZ0(TRACK2,Z0)
      DX_2 = Q(LFDCT2 + 7)
      DY_2 = Q(LFDCT2 + 8)
      X_2 = Q(LFDCT2 + 4) + DX_2*(0.0-Z0) 
      Y_2 = Q(LFDCT2 + 5) + DY_2*(0.0-Z0) 
C
      DEL_X = X_1-X_2
      DEL_Y = Y_1-Y_2
C
      A1 =  DX_1*DEL_X + DY_1*DEL_Y
      A2 = -DX_2*DEL_X - DY_2*DEL_Y
      B1 = 1.0 + DX_1**2 + DY_1**2
      B2 = 1.0 + DX_2**2 + DY_2**2
      C  = -(1.0 + DX_1*DX_2 + DY_1*DY_2)
      DENOM = B1*B2 - C**2
C
      IF ( DENOM.NE.0 ) THEN
        ZC1 = (A2*C - A1*B2) / DENOM
        XC1 = X_1 + DX_1*ZC1
        YC1 = Y_1 + DY_1*ZC1
C
        ZC2 = (A1*C - A2*B1) / DENOM
        XC2 = X_2 + DX_2*ZC2
        YC2 = Y_2 + DY_2*ZC2
      ELSE          ! Parallel lines, take point at Z=0.0
        ZC1 = 0.0
        XC1 = X_1
        YC1 = Y_1
C
        ZC2 = 0.0
        XC2 = X_2
        YC2 = Y_2
      ENDIF
      RC = (XC1-XC2)**2. + (YC1-YC2)**2. + (ZC1-ZC2)**2.
      RC = SQRT(RC)
C
  999 RETURN
      END
