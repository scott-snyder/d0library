C DEC/CMS REPLACEMENT HISTORY, Element GZMPED_N.FOR
C *1    10-FEB-1987 18:16:06 HEDIN "Muon constant bank get pointer routine"
C DEC/CMS REPLACEMENT HISTORY, Element GZMPED_N.FOR
      INTEGER FUNCTION GZMPED_N(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO NEW MUON PEDESTAL BANK FOR MODULE NMOD
CC
CC    HEDIN  1-20-87
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMPDH.LINK'
      INTEGER KSTPN,KSMUO,KMPDH,NMOD
C
      GZMPED_N=0
      KSTPN=LC(LSTPH-IZSTPN)
      IF(KSTPN.NE.0) THEN
        KSMUO=LC(KSTPN-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMPDH=LC(KSMUO-IZMPDH)
          IF(KMPDH.NE.0) THEN
            GZMPED_N=LC(KMPDH-NMOD)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
