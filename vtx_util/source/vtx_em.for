      SUBROUTINE VTX_EM(LCLUS,LVTXT,SIG1,SIG2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIND CLOSEST 2D VTXT MATCH TO EM CLUSTER
C-
C-   Inputs  : LCLUS = POINTER TO PELC OR PPHO
C-   Outputs : LVTXT = POINTER TO BEST VTXT
C-             SIG1 = RDPHI OF NEAREST VTXT-EM
C-             SIG2 = RDPHI OF SECOND TO NEAREST VTXT-EM
C-   Controls: 
C-
C-   Created   9-DEC-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCLUS,LVTXT
      REAL SIG1,SIG2
C
      INTEGER SAVE 
      REAL    ECR,X,Y,R,PHI,ECX,ECY,ECZ,DIS
C
      INTEGER GZVTXT
C----------------------------------------------------------------------
      SIG1 = 999.
      SIG2 = 999.
      SAVE = 0
      LVTXT = 0
      IF (LCLUS .LE. 0) GO TO 999
      ECX = Q(LCLUS+23)
      ECY = Q(LCLUS+24)
      ECZ = Q(LCLUS+25)
      ECR = SQRT(ECX**2+ECY**2)
      LVTXT = GZVTXT(0)
      DO WHILE (LVTXT .GT. 0)
        X = Q(LVTXT+7)
        Y = Q(LVTXT+8)
        R = SQRT(X*X+Y*Y)
        IF (X*ECX+Y*ECY .GT. 0.8*R*ECR) THEN
          PHI = Q(LVTXT+6)
          DIS = (ECY-Y)*COS(PHI) - (ECX-X)*SIN(PHI)
          IF (ABS(DIS) .LT. ABS(SIG1)) THEN
            SAVE = LVTXT
            SIG2 = SIG1
            SIG1 = DIS
          ELSEIF (ABS(DIS) .LT. ABS(SIG2)) THEN
            SIG2 = DIS
          ENDIF
        ENDIF
        LVTXT = LQ(LVTXT)
      ENDDO
      LVTXT = SAVE
  999 RETURN
      END          
