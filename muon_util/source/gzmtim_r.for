C DEC/CMS REPLACEMENT HISTORY, Element GZMTIM_R.FOR
C *1    10-FEB-1987 18:16:14 HEDIN "Muon constant bank get pointer routine"
C DEC/CMS REPLACEMENT HISTORY, Element GZMTIM_R.FOR
      INTEGER FUNCTION GZMTIM_R(NMOD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC    RETURNS POINTER TO REF MUON TIME     BANK FOR MODULE NMOD
CC
CC    HEDIN  1-20-87
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMTMH.LINK'
      INTEGER KSTPO,KSMUO,KMTMH,NMOD
C
      GZMTIM_R=0
      KSTPO=LC(LSTPH-IZSTPO)
      IF(KSTPO.NE.0) THEN
        KSMUO=LC(KSTPO-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMTMH=LC(KSMUO-IZMTMH)
          IF(KMTMH.NE.0) THEN
            GZMTIM_R=LC(KMTMH-NMOD)
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
