C DEC/CMS REPLACEMENT HISTORY, Element GZMGAN_N.FOR
C *1    10-FEB-1987 18:16:01 HEDIN "Muon constant bank get pointer routine"
C DEC/CMS REPLACEMENT HISTORY, Element GZMGAN_N.FOR
      INTEGER FUNCTION GZMGAN_N(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO NEW MUON GAIN     BANK FOR MODULE NMOD
CC
CC    HEDIN  1-20-87
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMGNH.LINK'
      INTEGER KSTPN,KSMUO,KMGNH,NMOD
C
      GZMGAN_N=0
      KSTPN=LC(LSTPH-IZSTPN)
      IF(KSTPN.NE.0) THEN
        KSMUO=LC(KSTPN-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMGNH=LC(KSMUO-IZMGNH)
          IF(KMGNH.NE.0) THEN
            GZMGAN_N=LC(KMGNH-NMOD)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
