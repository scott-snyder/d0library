      SUBROUTINE ISA_DUMP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Dump ISAJET information
C-
C-   ENTRY ISA_DEFD
C-    dialog to turn on/off ISAJET dump
C-
C-   Created   7-FEB-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PRUNIT,DMPUNI,LESUM,GZESUM
      LOGICAL YES
      SAVE YES
      DATA YES/.TRUE./
C----------------------------------------------------------------------
C
      IF(YES) THEN
        PRUNIT=DMPUNI()
        CALL PRTEVZ(PRUNIT)
        LESUM=GZESUM('ISAE')
        IF(LESUM.GT.0) CALL PRESUM ( PRUNIT, LESUM, 0, 'ONE', 0)
      ENDIF
C
      RETURN
C
      ENTRY ISA_DEFD
      YES=.TRUE.
      CALL GETPAR(1,' Dump ISAJET banks? [Y]:','L',YES)
  999 RETURN
      END
