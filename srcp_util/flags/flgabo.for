      SUBROUTINE FLGABO(MSG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Called by FLGBK if an error occurs
C-       Send message to screen indicating type of error
C-       Write list of booked flags to file FLAGS.LIST
C-       Abort.
C-
C-   Input:
C-   MSG = message indicating error
C-
C-   Created  26-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*80 MSG
      INTEGER ERR,LUN
      LOGICAL OK
C----------------------------------------------------------------------
C
      CALL INTMSG(MSG)
      CALL GTUNIT(1,LUN,ERR)
      CALL D0OPEN(LUN,'FLAGS.LIST','O',OK)
      WRITE(LUN,*) MSG
      CALL FLGPR(LUN)
      CALL ERRMSG('NO MORE ROOM FOR FLAGS',
     &  'FLGBK',' List of booked flags in file FLAGS.LIST','F')
  999 RETURN
      END
