      SUBROUTINE ETA_ZCORR(R,ZVTX,ETA,THETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate vertex corrected eta and theta 
C-                         given centroid position vector R(X,Y,Z) of 
C-                         cluster (assuming vertex Z = 0), and vertex Z.
C-
C-   Inputs  : R(X,Y,Z) (VTX=0), ZVTX
C-   Outputs : ETA, THETA
C-              
C-   Controls: NONE
C-
C-   Created   3-DEC-1992   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL R(3),ZVTX,THETA,ETA,ZPRIME,RPRIME
C----------------------------------------------------------------------
C
      ZPRIME = R(3)-ZVTX                          ! shift the origin
      RPRIME = SQRT(R(1)**2+R(2)**2+ZPRIME**2)    ! get new vector magnitude
      THETA = ACOS(ZPRIME/RPRIME)                 ! get theta
      ETA = -ALOG(MAX(TAN(THETA/2.0),1.E-9))      ! get eta
C
  999 RETURN
      END
