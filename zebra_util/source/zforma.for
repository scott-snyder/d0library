      FUNCTION ZFORMA( INPUT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert the input to hexadecimal representation
C-              to avoid Non fortran-77 Z format descriptor
C-
C-   Inputs  : INPUT, the variable to be coverted
C-   Outputs : the function result, character*8
C-
C-   Created  19-JAN-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*8  ZFORMA
      INTEGER INPUT, IMIN, IVAL, I
      CHARACTER*1  LABEL(0:15)
      DATA LABEL / '0','1','2','3','4','5','6','7',
     &             '8','9','A','B','C','D','E','F' /
C----------------------------------------------------------------------
      DO 10 I = 1, 8
        IMIN = 4*(8-I)
        IVAL = IBITS( INPUT, IMIN, 4 )
        ZFORMA(I:I) = LABEL( IVAL )
   10 CONTINUE
      DO 20 I = 1, 7
        IF( ZFORMA(I:I) .NE. '0' ) GOTO 999
        ZFORMA(I:I) = ' '
   20 CONTINUE
  999 RETURN
      END
