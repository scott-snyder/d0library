      SUBROUTINE PROCFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Use PATHST to specify whether bank should hang from
C-       'GEAN' or 'RECO'
C-
C-   Created   3-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LPROC
C--------------------------------------------------------------
      CALL BKPROC(LPROC)
  999 RETURN
      END
