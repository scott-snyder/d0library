      INTEGER FUNCTION GZMSHT( ITRAK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return MSHT bank pointer
C-
C-   Inputs  : MUOT track ID
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-FEB-1994   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  ITRAK, GZMUOT, LMUOT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMSHT.LINK'
C----------------------------------------------------------------------
      GZMSHT = 0
C
      LMUOT = GZMUOT(ITRAK)
      IF ( LMUOT.EQ.0 ) GOTO 999
C
      IF ( IQ(LMUOT-2).LT.3 ) GOTO 999
C
      GZMSHT = LQ(LMUOT-IZMSHT)
C
  999 RETURN
      END
