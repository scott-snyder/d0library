C DEC/CMS REPLACEMENT HISTORY, Element GZMGAN_R.FOR
C *1    10-FEB-1987 18:16:02 HEDIN "Muon constant bank get pointer routine"
C DEC/CMS REPLACEMENT HISTORY, Element GZMGAN_R.FOR
      INTEGER FUNCTION GZMGAN_R(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO REF MUON GAIN     BANK FOR MODULE NMOD
CC
CC    HEDIN  1-20-87
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMGNH.LINK'
      INTEGER KSTPO,KSMUO,KMGNH,NMOD
C
      GZMGAN_R=0
      KSTPO=LC(LSTPH-IZSTPO)
      IF(KSTPO.NE.0) THEN
        KSMUO=LC(KSTPO-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMGNH=LC(KSMUO-IZMGNH)
          IF(KMGNH.NE.0) THEN
            GZMGAN_R=LC(KMGNH-NMOD)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
