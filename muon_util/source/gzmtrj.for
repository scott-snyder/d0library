      INTEGER FUNCTION GZMTRJ(ITRAK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK MTRJ 
CC    'ITRAK' BANK 'MUON' TRACK NUMBER
CC
CC    HEDIN 12-3-89
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMTRJ.LINK'
      INTEGER I,ITRAK,LMUON,GZMUON
      GZMTRJ=0
      LMUON=GZMUON(ITRAK)
      IF(LMUON.NE.0) THEN
        GZMTRJ=LQ(LMUON-IZMTRJ)
      ENDIF
      RETURN
      END