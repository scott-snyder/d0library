      REAL FUNCTION MUCHIM(COFF,T,TQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : analytical form of the Chebychev derivative
C-
C-   Inputs  : COFF,T,TQ
C-   Outputs : MUCHIM
C-   Controls: 
C-
C-   Created  21-FEB-1992   A.Klatchko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I                
      REAL COFF(4),T,TQ 
C----------------------------------------------------------------------
      MUCHIM=COFF(4)*(12*TQ-3.0)       
      MUCHIM=MUCHIM+4*COFF(3)*T+COFF(2)
  999 RETURN
      END
