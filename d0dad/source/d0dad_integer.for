      SUBROUTINE D0DAD_INTEGER(STRINT,INTINT,ISTTYP)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: 
C-
C-   Inputs  : STRINT - Ascii (string) representation of an integer
C-   Outputs : INTINT - Integer equivalent
C-             ITYPE  - 0==>Decimal, 1==>Hex, -1==>Other
C-   Controls:
C-
C-   Created   7-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRINT
      INTEGER INTINT,ISTTYP,ITYPE,I,J
      REAL    VALUEX,RDUM
      INTEGER IVALUEX
      EXTERNAL VALUEX,IVALUEX
C-----------------------------------------------------------------------
      RDUM = VALUEX(STRINT,I,J,ITYPE)
      IF( ITYPE.EQ.1 .OR. ITYPE.EQ.7 ) INTINT=IVALUEX()
      ISTTYP=-1
      IF( ITYPE.EQ.1 ) ISTTYP=0
      IF( ITYPE.EQ.7 ) ISTTYP=1
  999 RETURN
      END
