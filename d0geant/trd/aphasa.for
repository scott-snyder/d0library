      FUNCTION APHASA(X,Y)
C     ---------======
C  RANDOM NUMBER BETWEEN X AND Y
C
      IMPLICIT NONE
      REAL X,Y,APHASA,RNDM
C
      APHASA=X+(Y-X)*RNDM(0)
      RETURN
      END
