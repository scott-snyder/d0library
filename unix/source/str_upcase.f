      INTEGER FUNCTION STR$UPCASE(OUTSTR, INSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Emulate the VAX RTL routine STR$UPCASE.
C-
C-   Returned value: 1 (.true.)
C-
C-   Inputs  : instr       Input string.
C-   Outputs : outstr      Output string
C-   Controls: None
C-
C-   Created   6-DEC-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      CHARACTER*(*) INSTR,OUTSTR
C----------------------------------------------------------------------
      OUTSTR = INSTR
      DO 10 I=1,LEN(OUTSTR)
 4       IF(LGE(OUTSTR(I:I),'a') .AND. LLE(OUTSTR(I:I), 'z'))
     &        OUTSTR(I:I) = 
     &        CHAR(ICHAR(OUTSTR(I:I)) + ICHAR('A') - ICHAR('a'))
 10   CONTINUE
      STR$UPCASE = 1
  999 RETURN
      END
