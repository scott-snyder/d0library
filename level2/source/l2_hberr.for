      SUBROUTINE L2_HBERR(ROUTINE,STRING,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display Error code seen by L2BOOK
C-
C-   Inputs  : ROUTINE = Error reported by routine name
C-             STRING  = User string describing Error
C-             ID      = Histogram Identifier
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-FEB-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) ROUTINE,STRING
      CHARACTER*80 MSG
      INTEGER ID
C----------------------------------------------------------------------
C
      WRITE(MSG,10)ROUTINE,ID,STRING
   10 FORMAT('L2_HBOOK Err, ',A8,' ID = ',I6.6,1x,A45)
C*DC*      CALL INTMSG(MSG)
      CALL ERRMSG('L2_HBOOK','L2_HBOOK',MSG,'W')
C
  999 RETURN
      END
