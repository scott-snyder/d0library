      SUBROUTINE UFLSET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Reset flags for PROCES
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      CALL FLGSET('PAUSE',.FALSE.)
      CALL FLGSET('EVENT_DISPLAY',.FALSE.)
      CALL FLGSET('WRITE_EVENT',.FALSE.)
      CALL FLGSET('DUMP_EVENT',.FALSE.)
      CALL FLGSET('EXAMINE',.FALSE.)
      CALL FLGSET('SUMMARIES',.FALSE.)
      CALL FLGSET('STATUS',.FALSE.)
      CALL FLGSET('DUMPF_REQ',.FALSE.)
      CALL FLGSET('DUMPH_REQ',.FALSE.)
      CALL FLGSET('DUMPU_REQ',.FALSE.)
  999 RETURN
      END
