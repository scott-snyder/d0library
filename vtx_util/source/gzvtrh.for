      INTEGER FUNCTION GZVTRH
C-------------------------------------------------------------------
C
C  Returns pointer to Zebra bank VTRH (header for VTX tracks
C
C    D.Z. MAY 1987                         
C                                  
C------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER LZTRH,GZZTRH,LVTRH 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZVTRH.LINK/LIST'
      GZVTRH=0
      LZTRH=GZZTRH()
      IF (LZTRH.NE.0) GZVTRH=LQ(LZTRH-IZVTRH)
      RETURN
      END   
