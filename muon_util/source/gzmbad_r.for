      INTEGER FUNCTION GZMBAD_R(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO REF MUON BAD BANK FOR MODULE NMOD
CC
CC    HEDIN  1-20-87
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMBHD.LINK'
      INTEGER KSTPO,KSMUO,KMBHD,NMOD
C
      GZMBAD_R=0
      KSTPO=LC(LSTPH-IZSTPO)
      IF(KSTPO.NE.0) THEN
        KSMUO=LC(KSTPO-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMBHD=LC(KSMUO-IZMBHD)
          IF(KMBHD.NE.0) THEN
            GZMBAD_R=LC(KMBHD-NMOD)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
