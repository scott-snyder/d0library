      INTEGER FUNCTION GZMDTM(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get the pointer to the current bank MDTM
C-                              for a muon module.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: NMOD - the module for which Delta times are required
C-
C-   Created  17-OCT-1988   Jim Green
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMDTH.LINK'
C----------------------------------------------------------------------
      INTEGER KSTPC,KSMUO,KMDTH,NMOD
C----------------------------------------------------------------------
      GZMDTM=0
      KSTPC=LC(LSTPH-IZSTPC)
      IF(KSTPC.NE.0) THEN
        KSMUO=LC(KSTPC-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMDTH=LC(KSMUO-IZMDTH)
          IF(KMDTH.NE.0) THEN
            GZMDTM=LC(KMDTH-NMOD)
          ENDIF
        ENDIF
      ENDIF

  999 RETURN
      END
