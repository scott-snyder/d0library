      SUBROUTINE INZGCB
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Init GCBANK (Geant Data Store)
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-NOV-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCBANK.INC'
      LOGICAL FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL GZEBRA(NGCBNK)
      ENDIF
  999 RETURN
      END
