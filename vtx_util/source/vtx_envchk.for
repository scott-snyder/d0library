      SUBROUTINE VTX_ENVCHK(VAL,PABS,T_DEGC,GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-DEC-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VAL(*),PABS,T_DEGC
      LOGICAL GOOD
      INTEGER I,TOT
      REAL SUM,PDIFF
C----------------------------------------------------------------------
      GOOD = .FALSE.
C
C ****  First 6 devices are temperatures -- require at least 2 of 6
C
      SUM = 0.
      TOT = 0
      DO I = 1,6
        IF (VAL(I) .GT. 0.) THEN
          TOT = TOT + 1
          SUM = SUM + VAL(I)
        ENDIF
      ENDDO
      IF (TOT .LT. 2) GO TO 999
      T_DEGC = SUM/TOT
C
C ****  Next three devices are static
C
      SUM = 0.
      TOT = 0
      DO I = 7,9
        IF(VAL(I) .GT. 0.) THEN
          TOT = TOT + 1
          SUM = SUM + VAL(I)
        ENDIF
      ENDDO
      IF (TOT .LT. 1)  GO TO 999
      PDIFF = SUM/TOT
C
C ****  Next device is barometric pressure
C
      PABS = VAL(10)
      IF (PABS .LT. 0.) GO TO 999
      PABS = PABS + 2.488*PDIFF     ! 1" w.c. = 2.488 mbar
      GOOD = .TRUE.
  999 RETURN
      END
