      SUBROUTINE PZTRK_CONV( PHI,XC,YC,
     &                       THETA,ZC,
     &                       X0,Y0,DX,DY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert ZTRK from center of gravity definition
C-              to simple Cartessian definition (relative to Z=0):
C-                      X = X0 + DX * Z
C-                      Y = Y0 + DY * Z
C-              If no theta information (THETA = 0), 
C-              return as if TAN(THETA) = 1.
C-
C-   Inputs  : PHI,XC,YC,THETA,Z0       Center of gravity track definition.
C-   Outputs : X0,Y0,DX,DY              Cartessian track definition.
C-
C-   Created   8-AUG-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Input:
      REAL    PHI,XC,YC
      REAL    THETA,ZC
C  Output:
      REAL    X0,Y0,DX,DY      
C  Local:
      REAL    TAN_THETA ,ZCENT 
C----------------------------------------------------------------------
C      
      IF ( THETA .EQ. 0 ) THEN
        TAN_THETA = 1.0
        ZCENT = 0
      ELSE
        TAN_THETA = TAN(THETA)
        ZCENT = ZC
      ENDIF
C
      DX = TAN_THETA * COS(PHI)
      DY = TAN_THETA * SIN(PHI)
      X0 = XC - ZC*DX
      Y0 = YC - ZC*DY
C
  999 CONTINUE
      RETURN
      END
