      LOGICAL FUNCTION UQUIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      User hook called before exiting, dummy version
C-
C-   Created  23-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      UQUIT=.TRUE.
      CALL INTMSG(' Dummy UQUIT called')
C----------------------------------------------------------------------
  999 RETURN
      END
