      SUBROUTINE POLANG(WX,WY,WZ,THETA,PHI)
C---------------------------------------------------------------------
C-                                                                   -
C-       calculate theta and phi from an unnormalized axis           -
C-                                                                   -
C-    INPUT:                                                         -
C-    WX,WY,WZ  the components of a ray                              -
C-                                                                   -
C-    OUTPUT:                                                        -
C-    THETA = polar coordinate of the axis , range 0 to PI           -
C-    PHI   = azimuthal coordinate of the axis range 0 to 2 PI       -
C-                                                                   -
C-                       JTL January, 1987                           -
C-                                                                   -
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      real *8 THETA,PHI
      real *8 WX,WY,WZ !unnormalized vector components
      REAL ANORM,ATEMP

      IF (WX.EQ.0.AND.WY.EQ.0) THEN
        IF(WZ.GE.0) THEN                    ! assign phi=0 for pos Z
       THETA = 0
       PHI = 0
        ELSE                                ! assign phi=180 for neg Z
       THETA = PI
        PHI = 0.
        END IF
      ELSE
       THETA = ATAN2( SQRT(WX**2 + WY**2), WZ)
       ATEMP = ATAN2( WY,WX )
       PHI =  ANORM ( ATEMP )
      ENDIF

      RETURN
      END
