      INTEGER FUNCTION GZMUCD(ITRAK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK MUCD 
CC    'ITRAK' BANK 'MUON' TRACK NUMBER
CC **********WARNING******************************************
CC   MUCD IS A LINEAR STRUCTURE. THIS POINTS TO THE FIRST BANK
CC ***********************************************************   
CC
CC    HEDIN 12-3-89
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUCD.LINK'
      INTEGER I,ITRAK,LMUON,GZMUON
      GZMUCD=0
      LMUON=GZMUON(ITRAK)
      IF(LMUON.NE.0) THEN
        GZMUCD=LQ(LMUON-IZMUCD)
      ENDIF
      RETURN
      END
