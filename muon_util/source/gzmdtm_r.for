      INTEGER FUNCTION GZMDTM_R(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get the pointer to the new bank MDTM 
C-                              for a muon module
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: NMOD - the module for which Delta times are required
C-
C-   Created  18-OCT-1988   Jim Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMDTH.LINK'
C----------------------------------------------------------------------
      INTEGER KSTPO,KSMUO,KMDTH,NMOD
C----------------------------------------------------------------------
      GZMDTM_R=0
      KSTPO=LC(LSTPH-IZSTPO)
      IF(KSTPO.NE.0) THEN
        KSMUO=LC(KSTPO-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMDTH=LC(KSMUO-IZMDTH)
          IF(KMDTH.NE.0) THEN
            GZMDTM_R=LC(KMDTH-NMOD)
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
