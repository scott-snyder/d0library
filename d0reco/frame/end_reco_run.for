      SUBROUTINE END_RECO_RUN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     End-of-run record read, do end of run summaries
C-
C-   Created   8-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      CALL RECO_RUN_SUMMARY_PBD
      CALL EVTWOS
  999 RETURN
      END
