      SUBROUTINE UPCASE (INSTR,OUTSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Conversion to uppercase. Uses KERNLIB routine
C-                         CLTOU or STR$UPCASE on a VAX machine.
C-
C-   Inputs  : instr       Input string.
C-   Outputs : outstr      Output string
C-   Controls: None
C-
C-   Created  19-JUN-1988   Harrison B. Prosper
C-   Updated  20-NOV-1988   Uses CERN library routine CLTOU
C-   Updated  20-APR-1989   Harrison B. Prosper   
C-                          Use D0FLAVOR to select faster VAX routine
C-                          otherwise select CERN routine
C-   Updated  27-APR-1989   K. Wyatt Merritt   correct argument in
C-                          STR$UPCASE
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) INSTR,OUTSTR
      INTEGER NUMCHR
C----------------------------------------------------------------------
      NUMCHR = LEN (INSTR)
C&IF VAXVMS
      CALL STR$UPCASE (OUTSTR,INSTR(1:NUMCHR))
C&ELSE
C&      OUTSTR = INSTR(1:NUMCHR)
C&      CALL CLTOU (OUTSTR(1:NUMCHR))
C&ENDIF
  999 RETURN
      END
