      FUNCTION GZFSEG(HALF,LAYER)
C------------------------------------------------------------------------  
C
C  Fetch the location of first FSGn bank in chain for layer=MODULE of 
C  Forward Drift Chamber 
C
C  Inputs: HALF,LAYER ->Layer 0=Inner Theta,Layer 1=Outer Theta,Layer 2=Phi
C                     
C  Output: =bank location           
C                     
C-   Created  19-JAN-1990   Jeffrey Bantly   
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format & layers 
C
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZFSEG
      INTEGER HALF,MODULE,LAYER
      INTEGER LKFTRH,GZFTRH
C------------------------------------------------------------------------
      GZFSEG = 0
      LKFTRH = GZFTRH()
      IF( LKFTRH .EQ. 0 ) GOTO 999
      MODULE = HALF*3 + LAYER
      GZFSEG = LQ(LKFTRH-3-MODULE)
C------------------------------------------------------------------------
  999 RETURN
      END
