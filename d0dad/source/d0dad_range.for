      SUBROUTINE D0DAD_RANGE(CINSTR,IRANGE)
C-----------------------------------------------------------------------
C  Parse a range of the form n1-n2 passed in CINSTR into the array 
C  IRANGE.  If CINSTR is of the form '-n', it is assumed to be a single
C  negative number.
C
C  Author:  John D. Hobbs
C  Date:    9-DEC-1993
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CINSTR
      INTEGER  IRANGE(2)
      INTEGER  IC
      INTEGER  LENOCC,ICFILA
      EXTERNAL LENOCC,ICFILA
C-----------------------------------------------------------------------
C
      IC=ICFILA('-',CINSTR,1,LENOCC(CINSTR))
      IF( IC.GT.LENOCC(CINSTR).AND.IC.NE.1 ) THEN
         READ(CINSTR,*) IRANGE(1)
      ELSE
         READ(CINSTR(1:IC-1),*) IRANGE(1)
         READ(CINSTR(IC+1:LENOCC(CINSTR)),*) IRANGE(2)
      ENDIF
C
 999  CONTINUE
      RETURN
      END
