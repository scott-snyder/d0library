      SUBROUTINE PXITOC( IEXP, NDIG, CEXP )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts an integer expression into a character
C-                         expression
C-
C-   Inputs  : 
C-   Outputs : 
C-
C-   Created   4-AUG-1986   Tami Kramer
C-   Updated   1-MAR-1988   Olivier Callot   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IEXP, NDIG, LRES
      CHARACTER*(*)CEXP
      CHARACTER*12 FORMA
C----------------------------------------------------------------------
      LRES = LEN( CEXP )
      IF( NDIG .LT. LRES ) THEN
        WRITE( FORMA, 1000 ) NDIG, LRES-NDIG
 1000   FORMAT('(I',I3,',',I3,'X)')
      ELSE
        WRITE( FORMA, 1010 ) LRES
 1010   FORMAT('(I',I3,')')
      ENDIF
      WRITE( CEXP, FORMA ) IEXP
  999 RETURN
      END
