      SUBROUTINE MGET_GEANT(FILN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Used To get GEANT stuff in muon package.
C-
C-
C-   Created  27-MAR-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILN
      INCLUDE 'D0$PARAMS:NBLANK.DEF'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      CALL INZGCB
      CALL MGINIT(FILN)
C---------------------------------------------------------------------
  999 RETURN
      END
