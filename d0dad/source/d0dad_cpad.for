      SUBROUTINE D0DAD_CPAD(CHRSTR)
C-----------------------------------------------------------------------
C  Pad a character string with blanks.
C
C  Author:    John D. Hobbs
C  Date:      19-DEC-1993
C
C  INPUTS:  CHRSTR -The string to be padded
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CHRSTR
      INTEGER I
      DO I=1,LEN(CHRSTR)
         IF( CHRSTR(I:I).LT.CHAR(20) ) CHRSTR(I:I)=' '
      ENDDO
      RETURN
      END
