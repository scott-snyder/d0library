      INTEGER FUNCTION GZMTOF(ITRAK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK MTOF 
CC    'ITRAK' BANK 'MUON' TRACK NUMBER
CC
CC    HEDIN 12-3-89
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMTOF.LINK'
      INTEGER I,ITRAK,LMUON,GZMUON
      GZMTOF=0
      LMUON=GZMUON(ITRAK)
      IF(LMUON.NE.0) THEN
        GZMTOF=LQ(LMUON-IZMTOF)
      ENDIF
      RETURN
      END
