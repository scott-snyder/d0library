      SUBROUTINE MKRECO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Construct RECO and its underlings HITS,PROC and HSTR
C-     Add also PARH (18-JAN-1989)
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZPARH
C----------------------------------------------------------------------
      CALL RECOFL
      CALL HITSFL
      CALL PROCFL
      CALL HSTRFL
      IF(GZPARH().EQ.0) CALL PARHFL
  999 RETURN
      END
