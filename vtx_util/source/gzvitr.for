      INTEGER FUNCTION GZVITR 
C----------------------------------------------------
C
C  Returns pointer to Zebra bank VITR 
C
C-   Created  24-OCT-1988   Ghita Rahal-Callot   
C                                        
C----------------------------------------------------
      IMPLICIT NONE                           
      INTEGER LVTRH, GZVTRH
      INCLUDE 'D0$LINKS:IZVITR.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      GZVITR=0
      LVTRH = GZVTRH(0)
      IF (LVTRH.NE.0) GZVITR=LQ(LVTRH-IZVITR) 
      RETURN
      END
