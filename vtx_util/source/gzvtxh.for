      INTEGER FUNCTION GZVTXH(DUMMY)
C----------------------------------------------------
C
C  Returns pointer to Zebra bank VTXH 
C
C    D.Z. MAR.,1987                         
C                                        
C----------------------------------------------------
      IMPLICIT NONE                           
      INTEGER DUMMY,LHITS,GZHITS
      INCLUDE 'D0$LINKS:IZVTXH.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      GZVTXH=0
      LHITS=GZHITS(0)
      IF (LHITS.NE.0) GZVTXH=LQ(LHITS-IZVTXH) 
      RETURN
      END
