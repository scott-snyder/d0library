      SUBROUTINE MOVPRT(CHARIN,NUM,OUTSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Move a character string from a part of CHARIN 
C-                         (array) to OUTSTR
C-
C-                         Used to pass the character string descriptor 
C-                         through another routine
C-
C-   Inputs  : CHARIN: Array to pick characters from
C-             NUM:    Number of element in CHARIN to choose
C-   Outputs : OUTSTR: String found
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHARIN(*),OUTSTR
      INTEGER NUM
C----------------------------------------------------------------------
      IF(CHARIN(NUM)(1:1).EQ.CHAR(0)) THEN
        OUTSTR=' '                     !Take care of null-filled strings
      ELSE
        OUTSTR=CHARIN(NUM)
      ENDIF
      RETURN
      END
