      INTEGER FUNCTION QCDRUNNO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Access to run number via runno() or
C-                         user setting if not working w/ zebra structure
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAY-1997   Bob Hirosky
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL zebra_override
      SAVE zebra_override
      DATA zebra_override /.false./
      INTEGER inrun, runnum, runno, qcdrunno_set
      SAVE runnum
C----------------------------------------------------------------------
      IF (zebra_override) THEN
        QCDRUNNO = runnum
      ELSE
        QCDRUNNO = runno()
      ENDIF
  999 RETURN
      ENTRY QCDRUNNO_SET(inrun)
      runnum = inrun
      zebra_override = .true.
      QCDRUNNO_SET = 0
      RETURN
      END
