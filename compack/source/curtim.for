      SUBROUTINE CURTIM(PFOUT,POSOUT,MAXIN,NUMIN) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read up and down cursor or PF-keys. VAX-specific.
C-
C-           It first read 3 characters from input, determines whether
C-           it is a cursor key or a PF-key or not. Only called in full
C-           screen mode.
C-           
C-           The read is done with a zero-time timeout here for use in AST mode
C-           to avoid stacked commands being passed to AST.
C-
C-   Inputs  : MAXIN: Maximum number of items in current menu.
C-             NUMIN: Number of columns in current display.
C-             POSOUT: Current position in menu display.
C-   Outputs : PFOUT: Number of PF-key struck (if any)
C-             POSOUT: New position in menu display
C-   Controls: None
C-
C-   Documented  22-SEP-1988   Jan S. Hoftun
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PFOUT,POSOUT,MAXIN,NUMIN
C
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL ISTAT,SMG$READ_KEYSTROKE
      INTEGER TCODE
      INTEGER*2 TCODE2
C----------------------------------------------------------------------
      ISTAT=SMG$READ_KEYSTROKE(KEYID,TCODE2,%VAL(0),0,MAINID,
     &                         %VAL(0),%VAL(0))
      TCODE = TCODE2
      IF(ISTAT) THEN
         CALL CHKCOM(TCODE,PFOUT,POSOUT,MAXIN,NUMIN)
      ELSE
         POSOUT=-1
      ENDIF
      RETURN
      END
