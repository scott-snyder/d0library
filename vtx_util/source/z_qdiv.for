      FUNCTION z_Qdiv(l,A1,A2,r1,r2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates Z coordinate
C-
C-   Inputs  :  l: layer
C-              A1,A2: Pulse areas for each end
C-              R1,R2: input impedances
C-
C-   Outputs :

C-   Controls:
C-
C-   Created  16-SEP-1992   John Hauptman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL R_PER_CM
      PARAMETER (R_per_cm = 18.)
      REAL el_half(0:2)
      REAL ALPHA,A2,A1,RWIRE
      REAL Z_QDIV,R2,R1
      INTEGER L
      DATA el_half / 48.3, 53.3, 58.4 /  ! cm,
C-----------------------------------------------------------------------
      alpha = (A2-A1) / (A2+A1)
      Rwire = R_per_cm * el_half(l) * 2.
C
      z_Qdiv = el_half(l) *
     &        ( alpha + ((1.+alpha)*r2-(1.-alpha)*r1)/Rwire)
  999 RETURN
      END
