      INTEGER FUNCTION STR$TRIM(OUTSTR, INSTR, OUTLEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Emulate the VAX RTL routine STR$TRIM
C-
C-   Returned value: 1 (.true.).
C-
C-   Inputs  : instr       Input string.
C-   Outputs : outstr      Output string
C-             outlen      Length of output string
C-   Controls: None
C-
C-   Created   6-DEC-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      CHARACTER*(*) INSTR,OUTSTR
      INTEGER D0_LOC
      INTEGER STRLEN
      INTEGER*2 OUTLEN
      CHARACTER*1 SPACE, TAB
      PARAMETER (SPACE = ' ', TAB = '	')
C----------------------------------------------------------------------
      DO 10 I=LEN(INSTR), 1, -1
        IF(INSTR(I:I).NE.SPACE .AND. INSTR(I:I).NE.TAB)GO TO 20
 10   CONTINUE
 20   CONTINUE
      STRLEN = MIN0(I, LEN(OUTSTR))
      OUTSTR = INSTR(1:STRLEN)
      IF(D0_LOC(OUTLEN).NE.0)OUTLEN = STRLEN
      STR$TRIM = 1
  999 RETURN
      END
