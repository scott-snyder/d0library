      FUNCTION JETFIX_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-JUL-1995   Dhiman Chakraborty
C-   Updated  26-OCT-1995   Dhiman Chakraborty   
C-                          JETFIX_RCP not needed anymore
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL JETFIX_INI,KTJET_INIT
      EXTERNAL KTJET_INIT
C----------------------------------------------------------------------
      JETFIX_INI = KTJET_INIT()
C
  999 RETURN
      END
