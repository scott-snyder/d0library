      SUBROUTINE VOLPOZ(IAR,NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets SRCPAR from array IAR and does volpos
C-                         Avoids direct SRCP.
C-
C-   Inputs  : IAR(*)  Array containing VOLPOS info.
C-             NUM     Number of words in IAR
C-   Outputs : 
C-   Controls: 
C-
C-   Created  19-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IAR(*),NUM
      INCLUDE 'D0$INC:SRCPR.INC'
      CALL UCOPY(IAR,ISRCPR,NUM)
      CALL VOLPOS1
  999 RETURN
      END
