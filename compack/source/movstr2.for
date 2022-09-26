      SUBROUTINE MOVSTR2(CHARIN,OUTSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Move a character string from CHARIN to OUTSTR
C-
C-                         Used to pass the character string descriptor 
C-                         through another routine
C-
C-   Inputs  : CHARIN: Input variable to get string from
C-   Outputs : OUTSTR: Styring found in CHARIN
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHARIN,OUTSTR
C----------------------------------------------------------------------
      IF(CHARIN(1:1).EQ.CHAR(0)) THEN
        OUTSTR=' '                     !TAKE CARE OF NULL FILLED STRINGS
      ELSE
        OUTSTR=CHARIN
      ENDIF
      RETURN
      END
