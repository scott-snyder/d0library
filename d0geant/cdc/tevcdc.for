      LOGICAL FUNCTION TEVCDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called before each event by LUTREV.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckheere
C-   Updated  17-JUL-1989   Harrison B. Prosper  
C-   Made into a pbd interface function 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      TEVCDC = .TRUE.
      IF ( DCDC .LT. 2 ) GOTO 999
C
  999 RETURN
      END
