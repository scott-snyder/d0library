      INTEGER FUNCTION GZMBAD_N(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO NEW MUON BAD BANK FOR MODULE NMOD
CC
CC    HEDIN  5-90
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMBHD.LINK'
      INTEGER KSTPN,KSMUO,KMBHD,NMOD
C
      GZMBAD_N=0
      KSTPN=LC(LSTPH-IZSTPN)
      IF(KSTPN.NE.0) THEN
        KSMUO=LC(KSTPN-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMBHD=LC(KSMUO-IZMBHD)
          IF(KMBHD.NE.0) THEN
            GZMBAD_N=LC(KMBHD-NMOD)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
