      FUNCTION DIRC(QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : right sign for torodial surfaces 
C-
C-   Returned value  : DIRC =-1./+1.
C-   Inputs  : QUAD 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-APR-1991   A.Klatchko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER QUAD
      REAL DIRC
C----------------------------------------------------------------------
      IF(QUAD.EQ.1.OR.QUAD.EQ.2)DIRC=1.0      !central +       
      IF(QUAD.EQ.3.OR.QUAD.EQ.4)DIRC=-1.0      !central -      
      IF(QUAD.GE.5.AND.QUAD.LE.8)DIRC=-1.0      !north end     
      IF(QUAD.GE.9.AND.QUAD.LE.12)DIRC=1.0      !south end     
  999 RETURN
      END
