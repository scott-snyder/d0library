      SUBROUTINE REFLECT_VECTORS( XS, YS, ZS, NS, IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : REFLECT VECTORS IN Z.  MERELY CHANGE
C-       ZS => -ZS.
C-
C-   Inputs  :     XS, YS, ZS    SIDE VECTORS -- DIMENSIONED (2, NS)
C-                 NS            NUMBER OF SIDES
C-   Outputs :     XS, YS, ZS
C-   Controls: 
C-
C-   Created  20-JAN-1990   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NS, IERR, I
      REAL XS(2, NS), YS(2, NS), ZS(2, NS)
C
      DO 100 I = 1, NS
      ZS( 1, I) = - ZS( 1, I)
      ZS( 2, I) = - ZS( 2, I)
  100 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
