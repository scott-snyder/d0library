      INTEGER FUNCTION GZMHIT(ITRAK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK MHIT 
CC    'ITRAK' BANK 'MUON' TRACK NUMBER
CC
CC    HEDIN 12-3-89
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMHIT.LINK'
      INTEGER I,ITRAK,LMUON,GZMUON
      GZMHIT=0
      LMUON=GZMUON(ITRAK)
      IF(LMUON.NE.0) THEN
        GZMHIT=LQ(LMUON-IZMHIT)
      ENDIF
      RETURN
      END