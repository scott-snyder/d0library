      FUNCTION ISAOBR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Print out to USER.OUT begin-of-run record
C-
C-   Created   6-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISAOBR
      INTEGER USUNIT,PRUNIT
C----------------------------------------------------------------------
C
      ISAOBR=.TRUE.
      PRUNIT=USUNIT()
      CALL PRISAB(PRUNIT,0,0,0,0) 
  999 RETURN
      END
