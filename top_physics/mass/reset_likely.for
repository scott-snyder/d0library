      SUBROUTINE RESET_LIKELY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset Likelihood to zero
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-JUN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INTEGER I1,I2,I3
C----------------------------------------------------------------------
      DO I3 = 1 , 2
        DO I2 = 1 , 2
          DO I1 = 1 , MAXT
            LIKELY(I1,I2,I3) = 0.0D0
          ENDDO
        ENDDO
      ENDDO
  999 RETURN
      END
