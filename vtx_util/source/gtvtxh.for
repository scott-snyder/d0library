      SUBROUTINE GTVTXH(NHIT)
C-----------------------------------------------------------------  
C          
C  Fetch contents of Zebra bank VTXH 
C
C  Output: NHIT  = number of hits in Vertex Chamber   
C                     
C  Daria Zieminska FEB., 1987
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER NHIT,LVTXH,GZVTXH
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C
      NHIT=0
      LVTXH=GZVTXH(0)
      NHIT=IQ(LVTXH+1)
      RETURN
      END
