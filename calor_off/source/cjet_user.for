      FUNCTION CJET_USER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Shell for user algorithm jet finder.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-OCT-1989   Gerald C. Blazey, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST,OK,CJET_USER
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
      END IF
C
C ****  
C
      CJET_USER = .TRUE.
C
C ****  
C
  999 RETURN
      END
