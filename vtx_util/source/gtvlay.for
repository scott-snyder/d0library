      SUBROUTINE GTVLAY(LAYER,NHIT)
C-----------------------------------------------------------------  
C
C  Fetch contents of Zebra bank VLAY (bank of hits in a VTX layer)
C
C  Input : LAYER = layer number   
C  Output: NHIT  = number of hits in LAYER   
C                     
C  Daria Zieminska FEB. ,1987
C
C-----------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER LAYER,NHIT,LVLAY,GZVLAY
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C
      NHIT=0
      LVLAY=GZVLAY(LAYER)
      NHIT=IQ(LVLAY+1)
      RETURN
      END
