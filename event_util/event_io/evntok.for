      LOGICAL FUNCTION EVNTOK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Return TRUE if record read is a good event
C-
C-   Created  30-MAR-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL OK
C----------------------------------------------------------------------
      EVNTOK=.FALSE.
      IF(LHEAD.NE.0) EVNTOK=MOD(IQ(LHEAD+1),1000).GT.4
  999 RETURN
      END
