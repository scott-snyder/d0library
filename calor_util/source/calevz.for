
      FUNCTION CALEVZ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     zero all pointers before next event is read in
C-     to be called by event zeroing user hook
C-   Returned value  : true
C-
C-   Created  26-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CALEVZ
C----------------------------------------------------------------------
      CALEVZ=.TRUE.
      CALL CPTCAZ    ! zero PTCAEP
      CALL CPTCTZ    ! zero PTCATE
  999 RETURN
      END
