      SUBROUTINE MKRECO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Construct RECO and its underlings HITS,PROC and HSTR
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL RECOFL
      CALL HITSFL
      CALL PROCFL
      CALL HSTRFL
  999 RETURN
      END
