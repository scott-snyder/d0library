      SUBROUTINE GTWGHT(WEIGHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the event weight from the WGHT bank.
C-                         Returns 1. if WGHT does not exist.
C-
C-   Inputs:
C-   Outputs : WEIGHT - Event weight
C-
C-   Created  19-Apr-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LWGHT, GZWGHT
      REAL WEIGHT
C----------------------------------------------------------------------
      LWGHT = GZWGHT()
      IF(LWGHT.EQ.0)THEN
        WEIGHT = 1.
      ELSE
        WEIGHT = Q(LWGHT+2)
      ENDIF
  999 RETURN
      END
