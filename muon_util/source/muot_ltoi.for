      INTEGER FUNCTION MUOT_LTOI(LMUOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : give index of muot bank corresponding 
C-                         to pointer LMUOT (inverse of GZMUOT(IMUOT))
C-
C-   Returned value  : IMUOT, where GZMUOT(IMUOT) = LMUOT
C-   Inputs  : LMUOT
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-AUG-1992   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LMUOT,IMUOT,GZMUOT,NMUOT
      INTEGER LMUOT_TEST
      EXTERNAL GZMUOT
C----------------------------------------------------------------------
      MUOT_LTOI = 0
      CALL GTMTRH(NMUOT)
      IF(NMUOT.GT.0) THEN
        DO IMUOT=1,NMUOT
          LMUOT_TEST = GZMUOT(IMUOT)
          IF(LMUOT_TEST.EQ.LMUOT) MUOT_LTOI = IMUOT
        ENDDO
      ENDIF  
  999 RETURN
      END
