      FUNCTION ZEBCOM_SPACE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-MAY-1995   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NEED
      LOGICAL ZEBCOM_SPACE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C----------------------------------------------------------------------
      ZEBCOM_SPACE = .FALSE.
      CALL MZNEED(IXMAIN, NEED, 'G')
      ZEBCOM_SPACE = IQUEST(11).GT.0
  999 RETURN
      END
