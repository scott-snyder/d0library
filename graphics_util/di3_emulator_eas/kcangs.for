      SUBROUTINE KCANGS(PHI, TET, PSI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To calculate the rotation angles for text
C-                          manipulations using Eurelerian angles.
C-
C-   Inputs  : None
C-   Outputs : PHI, TET, PSI : Right handed Eurelerian rotation angles
C-                             in degrees.
C-   Controls: None
C-
C-   Created  16-MAR-1989   SHAHRIAR ABACHI
C-   Updated  24-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    PHI, PSI, TET
      INCLUDE 'D0$INC:TEXATT.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:pi.def'
C- LOCAL VARIABLES:
      REAL CXB, CYB, CZB, CXP, CYP, CZP, CXD, CYD, CZD
      REAL D0, D1, D2, D3, COSPSI, COSPHI, EPS
      REAL LEFT, ETX, ETY, CNV, NORM
      REAL B(3), P(3), D(3)
      DATA CNV, EPS /57.2957795, 1.E-09/
C
      LEFT = - RIGHT
C
C--- Normalized base vector in right handed coordinate system:
C
      NORM = SQRT( CXBASE*CXBASE + CYBASE*CYBASE + CZBASE*CZBASE)
      CXB = CXBASE / NORM
      CYB = CYBASE / NORM
      CZB = (CZBASE / NORM) * LEFT
C
C--- Normalized plane vector in right handed coordinate system :
C
      NORM = SQRT( CXPLAN*CXPLAN + CYPLAN*CYPLAN + CZPLAN*CZPLAN)
      CXP = CXPLAN / NORM
      CYP = CYPLAN / NORM
      CZP = (CZPLAN / NORM) * LEFT
C
C--- Normal to BASE and PLANE (DEPTH VECTOR) :
C
      CXD = CYP * CZB - CZP * CYB
      CYD = CZP * CXB - CXP * CZB
      CZD = CXP * CYB - CYP * CXB
      NORM = SQRT( CXD*CXD + CYD*CYD + CZD*CZD)
      CXD = CXD / NORM
      CYD = CYD / NORM
      CZD = CZD / NORM
C
C--- Correct direction of plane vector to form a left handed coordinate
C--- system with base and depth vectors.
C
      CXP = CYB * CZD - CZB * CYD
      CYP = CZB * CXD - CXB * CZD
      CZP = CXB * CYD - CYB * CXD
C
C--- Put every thing back in left handed system :
C
      CZB = - CZB
      CZP = - CZP
      CZD = - CZD
C***********************
      B(1)=CXB
      B(2)=CYB
      B(3)=CZB
      P(1)=CXP
      P(2)=CYP
      P(3)=CZP
      D(1)=CXD
      D(2)=CYD
      D(3)=CZD
C***********************
C
C--- Take a simple case :
C
      NORM = SQRT(CXD * CXD + CYD * CYD)
      IF( NORM .LT. EPS ) THEN
        PSI = 0.0
        TET = 0.0
        IF(CZD .LT. 0.0) TET = 180.0
        PHI = - CNV * ACOS(CXB) * SIGN(1.0, CYB)
        GOTO 999
      ENDIF
C
C--- Genaral case :
C--- ETX & ETY :  X,Y projections of a vector along intersection of X/Y
C---              and BASE/PLANE planes.
C
      ETX = CYD / NORM
      ETY = -CXD / NORM
      D1 = ETX * CXB + ETY * CYB
      IF(D1 .LT. 0.0) THEN
        ETX = -ETX
        ETY = -ETY
      ENDIF
      PHI = - CNV * ACOS(ETX) * SIGN(1.0, ETY)
C
      D1 = ETX * CXB + ETY * CYB
      D2 = ETX * CXP + ETY * CYP
      PSI =  CNV * ACOS(D1) * SIGN(1.0, D2)
C
      D3 = CXD * SIN(PHI*pi/180) + CYD * COS(PHI*pi/180)
      TET = CNV * ASIN(D3)
      IF(CZD .LT. 0.0) TET = 180.0 - TET
C
  999 RETURN
      END
