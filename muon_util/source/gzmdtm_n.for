      INTEGER FUNCTION GZMDTM_N(NMOD)
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
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZMDTH.LINK'
C----------------------------------------------------------------------
      INTEGER KSTPN,KSMUO,KMDTH,NMOD
C----------------------------------------------------------------------
      GZMDTM_N=0
      KSTPN=LC(LSTPH-IZSTPN)
      IF(KSTPN.NE.0) THEN
        KSMUO=LC(KSTPN-IZSMUO)
        IF(KSMUO.NE.0) THEN
          KMDTH=LC(KSMUO-IZMDTH)
          IF(KMDTH.NE.0) THEN
            GZMDTM_N=LC(KMDTH-NMOD)
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
