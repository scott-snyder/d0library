      SUBROUTINE HEX_TO_SINT(CHAR,VAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert hex to sign integer
C-
C-   Inputs  : CHAR(*)  C
C-   Outputs : VAL      I
C-   Controls: 
C-
C-   Created  29-JUN-1994   Johannes V. (Djoko) Wirjawan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER CHAR*(*)
      INTEGER NCHAR,VAL,INT,MAX
C
      NCHAR = LEN(CHAR)
      CALL HEX_TO_INT(CHAR, INT)
      MAX = (16**NCHAR)/2
      IF (INT.LE.MAX) THEN
         VAL = INT
      ELSE
         VAL = INT-2*MAX
      ENDIF      
C----------------------------------------------------------------------
  999 RETURN
      END
