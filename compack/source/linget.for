      INTEGER FUNCTION LINGET(LINE,OUTSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get current text on a given line.
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE:   Line to get text on.
C-   Outputs : OUTSTR: String found on line
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated  1-OCT-1991  Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LINE
      CHARACTER*(*) OUTSTR
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL ISTAT,SMG$READ_FROM_DISPLAY
C----------------------------------------------------------------------
      ISTAT=SMG$READ_FROM_DISPLAY(MAINID,OUTSTR,%VAL(0),LINE)
      IF(.NOT.ISTAT) THEN
         CALL MSGSCR(ISTAT,'LINGET-->')
      ENDIF
      LINGET=ISTAT
      RETURN
      END
