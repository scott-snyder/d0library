      SUBROUTINE ZEROEV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Call any subroutines zeroing common arrays per event
C-
C-   Created  25-NOV-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL PTCAZR      ! zero PTCAEH array
  999 RETURN
      END
