      SUBROUTINE EXTRAP(X1,Y1,X2,Y2,X0,Y0)
C     -----------======------------------
C  LINEAR INTER(EXTRA)POLATION
C
      IMPLICIT NONE
      REAL X0,X1,X2,Y0,Y1,Y2
C
      Y0=Y1
      IF(X1.EQ.X2.OR.Y1.EQ.Y2)RETURN
      Y0=Y1+(Y1-Y2)*(X0-X1)/(X1-X2)
      RETURN
      END
