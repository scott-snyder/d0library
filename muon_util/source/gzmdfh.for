      INTEGER FUNCTION GZMDFH(I)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO MUON TIME-DISTANCE HEADER               
CC
CC    HEDIN  9-91
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMDFH.LINK'
      INTEGER KSTPC,KSMUO,KMDFH,I   
C
      GZMDFH=0
      KSTPC=LC(LSTPH-IZSTPC)
      IF(KSTPC.NE.0) THEN
        KSMUO=LC(KSTPC-IZSMUO)
        IF(KSMUO.NE.0) THEN
          GZMDFH=LC(KSMUO-IZMDFH)
        ENDIF
      ENDIF
      RETURN
      END
