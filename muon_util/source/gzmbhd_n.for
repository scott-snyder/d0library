      INTEGER FUNCTION GZMBHD_N(I)
CC-----------------------------------------------------------------
CC    RETURNS POINTER TO MUON BAD HEADER               
CC
CC    C.Francis  Aug. 90
CC-----------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMBHD.LINK'
      INTEGER KSTPN,KSMUO,KMBHD,I   
CC-----------------------------------------------------------------
      GZMBHD_N=0
      KSTPN=LC(LSTPH-IZSTPN)
      IF(KSTPN.NE.0) THEN
        KSMUO=LC(KSTPN-IZSMUO)
        IF(KSMUO.NE.0) THEN
          GZMBHD_N=LC(KSMUO-IZMBHD)
        ENDIF
      ENDIF
      RETURN
      END
