      INTEGER FUNCTION GZMUON(ITRAK)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO ZEBRA BANK MUON 
CC    MUON IS A LINEAR BANK, THIS POINTS TO 
CC    TRACK ITRAK (OR TRACK 1 IF ITRAK =0) 
CC    WHERE ITRAK IS THE 'ITRAK' TRACK IN LINESR STRUCTURE
CC
CC    HEDIN 12-3-89
CC    KUNORI 29-MAR-90  USE 'LZFIND'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUON.LINK'
      INTEGER I,ITRAK,LMTRH,GZMTRH
      INTEGER LZFIND
      EXTERNAL LZFIND
C
      GZMUON=0
      LMTRH=GZMTRH(0)
      IF(LMTRH.NE.0) THEN
        IF(ITRAK.LE.0) THEN
          GZMUON=LQ(LMTRH-IZMUON)
        ELSE
          I=ITRAK
          GZMUON=LZFIND(IXCOM,LMTRH-IZMUON,I,-5) 
        ENDIF
C
      ENDIF
      RETURN
      END