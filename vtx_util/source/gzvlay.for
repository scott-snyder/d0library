      INTEGER FUNCTION GZVLAY(LAYER)
C------------------------------------------------------
C
C  Returns pointer to Zebra bank VLAY 
C  Input:
C    LAYER = vertex chamber layer (values 0,1,2)
C
C    D.Z. MAR.,1987                         
C                                        
C------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER LAYER,LVTXH,GZVTXH
      INCLUDE 'D0$LINKS:IZVLAY.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      GZVLAY=0
      LVTXH=GZVTXH(0)
      IF (LVTXH.NE.0) GZVLAY=LQ(LVTXH-IZVLAY-LAYER) 
      RETURN
      END
