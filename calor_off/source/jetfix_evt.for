      LOGICAL FUNCTION JETFIX_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-JUL-1995   Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      LOGICAL KTJET_RUN
      EXTERNAL KTJET_RUN
C----------------------------------------------------------------------
      JETFIX_EVT = KTJET_RUN()
  999 RETURN
      END
