C DEC/CMS REPLACEMENT HISTORY, Element GZMGAN.FOR
C *1    10-FEB-1987 18:15:59 HEDIN "Muon constant bank get pointer routine"
C DEC/CMS REPLACEMENT HISTORY, Element GZMGAN.FOR
      INTEGER FUNCTION GZMGAN(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO MUON GAINS    BANK FOR MODULE NMOD
CC
CC    HEDIN  1-20-87
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMGNH.LINK'
      INTEGER KSTPC,KSMUO,KMGNH,NMOD
C
      GZMGAN=0
      KSTPC=LC(LSTPH-IZSTPC)
      IF(KSTPC.NE.0) THEN
        KSMUO=LC(KSTPC-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMGNH=LC(KSMUO-IZMGNH)
          IF(KMGNH.NE.0) THEN
            GZMGAN=LC(KMGNH-NMOD)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
