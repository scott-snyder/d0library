      FUNCTION ISAOER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Print out to USER.OUT end-of-run record
C-
C-   Created   6-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISAOER
      INTEGER USUNIT,PRUNIT
C----------------------------------------------------------------------
C
      ISAOER=.TRUE.
      PRUNIT=USUNIT()
      CALL PRISAF(PRUNIT,0,0,0,0) 
  999 RETURN
      END
