      INTEGER FUNCTION OSTRM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Give output stream being written
C-
C-     ENTRY SOSTRM(I)
C-   Inputs  : 
C-      I=  output stream
C-
C-   Created   7-MAR-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SOSTRM,I,ISET
C
      OSTRM=ISET
      RETURN
C
      ENTRY SOSTRM(I)
      ISET=I
C----------------------------------------------------------------------
  999 RETURN
      END
