      SUBROUTINE DMPREQ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     set dump request flags
C-
C-   Created   1-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FLGVAL
C----------------------------------------------------------------------
C
      IF(FLGVAL('DUMP_PROCES')) CALL FLGSET('DUMPF_REQ',.TRUE.)
      IF(.NOT.FLGVAL('DUMP_NONE_H')) CALL FLGSET('DUMPH_REQ',.TRUE.)
      IF(FLGVAL('DUMP_USER')) CALL FLGSET('DUMPU_REQ',.TRUE.)
C
  999 RETURN
      END
