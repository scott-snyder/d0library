      SUBROUTINE DIRCOS(DIR,SLDA,SLDBC,SLWA,SLWBC,Pa,Da,Wa,Pb,Db,Wb)  
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : determine direction cosines signs
C-
C-   Inputs  : 
C-        SLDA,SLDBC,SLWA,SLWBC - SLOPES IN WIRE & DRIFT DIRECTIONS
C-   Outputs : Pa,Da,Wa,Pb,Db,Wb - direction cosines
C-   Controls: 
C-
C-   Created  18-APR-1991   A.Klatchko
C-   Updated  18-JUN-1991   AKL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL DIR,Pa,Da,Wa,Pb,Db,Wb
      REAL SLDA,SLDBC,SLWA,SLWBC
C----------------------------------------------------------------------
        Pa=DIR/sqrt(1.+SLDA*SLDA+SLWA*SLWA)
        Da=SLDA*Pa                                                 
        Wa=SLWA*Pa                                                 
        Pb=DIR/sqrt(1.+SLDBC*SLDBC+SLWBC*SLWBC)      
        Db=SLDBC*Pb                                                
        Wb=SLWBC*Pb                                                
  999 RETURN
      END
