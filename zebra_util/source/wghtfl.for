      SUBROUTINE WGHTFL(WEIGHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book a WGHT bank and store the weight.
C-
C-   Inputs:   WEIGHT - Event weight
C-   Outputs:
C-
C-   Created  19-Apr-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LWGHT
      REAL WEIGHT
C----------------------------------------------------------------------
      CALL BKWGHT(LWGHT)
      IF(LWGHT.EQ.0)RETURN
      IQ(LWGHT+1) = 1
      Q(LWGHT+2) = WEIGHT 
  999 RETURN
      END
