      SUBROUTINE QCD_L1_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill our psuedo arrays with ET's from the 
C-                         trigger block
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-JUL-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PSL1.INC'
      INTEGER I,J, IETA
      REAL EMET, TOTET
C----------------------------------------------------------------------
      CALL VZERO( PSL1TOW, 40*32 )
      CALL VZERO( PSL1LT, 10*4 )
C
C: Fill arrays with ET values
C
      DO I = 1, 32
        DO J = 1, 40
          IF ( J .GT. 20 ) THEN
            IETA = J - 20
          ELSE
            IETA = J - 21
          ENDIF
          CALL CL1PHET(I, IETA, EMET, TOTET )
          PSL1TOW(J, I ) = IFIX((TOTET+2.0)/.50)*.50 - 2.00
        ENDDO
      ENDDO
  999 RETURN
      END
