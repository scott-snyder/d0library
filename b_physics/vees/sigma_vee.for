      SUBROUTINE SIGMA_VEE(LPVES,SIGMA_PHI_VEE,SIGMA_THETA_VEE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate errors of Vee angles 
C-   Input:     LPVES
C-   Outputs:   SIGMA_PHI_VEE,SIGMA_THETA_VEE
C-
C-   Created  29-SEP-1991   Vladimir Burtovoy
C-   Updated   7-NOV-1991   Daria Zieminska  combined two routines 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LPVES,GZPVES,LVERT,LVERTP,GZVERT
      LOGICAL OK
      REAL X,Y,Z,DX,DY,DZ,XV,YV,ZV,DXV,DYV,DZV,THETA,THETA_VEE,DT
      REAL PHI,PHI_VEE,DP,SIGMA_PHI_VEE,SIGMA_THETA_VEE
C
            LVERT   = LQ(LPVES-2)
            LVERTP  = GZVERT(1)       
            X   = Q(LVERTP +  3)      ! coordinates of primary vertex
            Y   = Q(LVERTP +  4)
            Z   = Q(LVERTP +  5)
            DX  = Q(LVERTP +  6)
            DY  = Q(LVERTP +  7)
            DZ  = Q(LVERTP +  8)
            XV  = Q(LVERT  +  3)      ! coordinates of vee
            YV  = Q(LVERT  +  4)
            ZV  = Q(LVERT  +  5)
            DXV = Q(LVERT  +  6)
            DYV = Q(LVERT  +  7)
            DZV = Q(LVERT  +  8)
            PHI = PHI_VEE(X,Y,XV,YV)
      DP = (PHI_VEE(X + DX,Y, XV, YV) - PHI)**2 +
     &     (PHI_VEE(X,Y + DY, XV, YV) - PHI)**2 +
     &     (PHI_VEE(X,Y,XV + DXV, YV) - PHI)**2 +
     &     (PHI_VEE(X,Y,XV, YV + DYV) - PHI)**2 
            SIGMA_PHI_VEE = SQRT(DP)
            THETA = THETA_VEE(X,Y,Z,XV,YV,ZV)
      DT = (THETA_VEE(X + DX,Y,Z, XV, YV, ZV) - THETA)**2 +
     &     (THETA_VEE(X,Y + DY,Z, XV, YV, ZV) - THETA)**2 +
     &     (THETA_VEE(X,Y,Z + DZ, XV, YV, ZV) - THETA)**2 +
     &     (THETA_VEE(X,Y,Z,XV + DXV, YV, ZV) - THETA)**2 +
     &     (THETA_VEE(X,Y,Z,XV, YV + DYV, ZV) - THETA)**2 +
     &     (THETA_VEE(X,Y,Z,XV, YV, ZV + DZV) - THETA)**2 
            SIGMA_THETA_VEE = SQRT(DT)
      RETURN
      END
